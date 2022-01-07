module OpenEtw.Tests.GenerateHeader
open OpenEtw
open OpenEtw.Util

type GuidFormat =
    | PlainDigits
    | HyphenatedDigits
    | HyphenatedDigitsWithBraces
    | HyphenatedDigitsWithParentheses
    override this.ToString() =
        match this with
        | PlainDigits -> "N"
        | HyphenatedDigits -> "D"
        | HyphenatedDigitsWithBraces -> "B"
        | HyphenatedDigitsWithParentheses -> "P"

type HeaderOptions =
    {
        implicitEventNumbering : bool
        elideHashedProviderGuid : bool
        guidFormat : GuidFormat
    }

let generateHeader (provider : EtwProvider) (options : HeaderOptions) =
    let formatGuid (guid : System.Guid) = guid.ToString(options.guidFormat.ToString())
    let joinLines = List.where ((<>) "") >> String.concat System.Environment.NewLine
    let per collection f = collection |> List.map f |> joinLines
    let eventsByTask = provider.events |> List.groupBy (fun e -> e.task)
    let tasksByName = provider.tasks |> List.map (fun t -> t.name, t) |> Map.ofList

    [
        """#pragma once
#include <windows.h>
#include "etw.h"
"""
        (if (options.elideHashedProviderGuid && providerToGuid provider.name = provider.guid) then
            sprintf """ETW_PROVIDER_BEGIN(%s%s)""" 
                provider.className
                (if provider.className <> provider.name then sprintf ", name=\"%s\"" provider.name else "")
            else
            sprintf 
                """ETW_PROVIDER_BEGIN(%s%s, guid="%s")"""
                provider.className
                (if provider.className <> provider.name then sprintf ", name=\"%s\"" provider.name else "")
                (formatGuid provider.guid))

        per provider.maps (fun map ->
            let perElem f = map.elements |> List.map f |> String.concat ("," + System.Environment.NewLine)
            let mapTypeString = 
                match map.mapType with
                | ValueMap -> "VALUE"
                | BitMap -> "BIT"
            [
                (sprintf 
                    "    ETW_%sMAP(%s%s)"
                    mapTypeString
                    map.name
                    (if (map.prefix.IsNone) then "" else
                        sprintf ", prefixToIgnore=\"%s\"" map.prefix.Value))

                perElem (fun (name, value) -> sprintf "        %s%s = %d" (map.prefix |?? "") name value)

                "    ETW_MAP_END"
            ] |> joinLines
        )

        per provider.keywords (fun k ->
            if (k.implicit) then "" else
            sprintf "    ETW_KEYWORD(0x%x, %s, symbol=\"%s\"%s)" 
                k.id 
                k.name 
                k.symbol 
                (if k.message.IsNone then "" else
                    sprintf ", message=\"%s\"" k.message.Value))

        per provider.opcodes (fun o ->
            if (o.implicit) then "" else
            sprintf "    ETW_OPCODE(%d, %s, symbol=\"%s\"%s)" 
                o.id 
                o.name 
                o.symbol 
                (if o.message.IsNone then "" else
                    sprintf ", message=\"%s\"" o.message.Value))

        // TODO: Levels
        // TODO: Channels

        // Task-less events
        // Events by task
        per eventsByTask (fun (maybeT, events) ->
            let formatParam (p:EtwEventParam) =
                let count = 
                    match p.count with
                    | EtwCount.Single -> ""
                    | EtwCount.Fixed len -> sprintf "ETW_COUNT(%d) " len
                    | EtwCount.Counted param ->  sprintf "ETW_COUNT(%s) " param

                let inType = sprintf "ETW_IN(%s) " (p.inType.HeaderType())
                let outType = sprintf "ETW_OUT(%s) " (p.outType.HeaderType())
                let length = 
                    match p.inType.Length() with
                    | Some EtwLength.NullTerminated -> ""
                    | Some (EtwLength.Fixed length) -> sprintf "ETW_LEN(%d) " length
                    | Some (EtwLength.Counted param) -> sprintf "ETW_LEN(%s) " param
                    | None -> ""

                sprintf "%s%s%s%s%s %s"
                    count
                    length
                    inType
                    outType
                    p.cppType
                    p.name

            let formatEvent e =
                sprintf 
                    """    ETW_EVENT(%s, %s)(%s);"""
                    e.cppName
                    ([
                        // If emitting ids...
                        yield "id", e.id.Value.ToString()
                        if (e.name <> e.cppName) then
                            yield "name", e.name
                        if (e.symbol <> e.cppName) then
                            yield "symbol", e.symbol
                        if (e.message.IsSome) then
                            yield "message", e.message.Value
                        yield "version", e.version.ToString()
                        if (e.opcode.IsSome) then
                            yield "opcode", e.opcode.Value
                        if (List.isEmpty e.keywords |> not) then
                            yield "keywords", (e.keywords |> String.concat ", ")
                        if (e.level.IsSome) then
                            yield "level", e.level.Value
                    ] |> List.map (fun (name, value) -> sprintf "%s=\"%s\"" name value) |> String.concat ", ")
                    (e.parameters |> List.map formatParam |> String.concat ", ")

            let allEvents = events |> List.map formatEvent |> joinLines

            match maybeT with
            | Some taskName ->
                let t = Map.find taskName tasksByName 
                
                [ 
                    (sprintf "    ETW_TASK_BEGIN(%d, \"%s\"%s)"
                        t.id t.name
                        (if t.guid.IsNone then "" else sprintf ", guid=\"%s\"" (formatGuid t.guid.Value)))

                    allEvents
                    "    ETW_TASK_END" ] |> joinLines
            | None -> allEvents
        )

        "ETW_PROVIDER_END"
        ""
//        """
//    ETW_TASK_BEGIN(1, "GUI", guid="{8D9DD199-B7C8-4F3C-8F77-96FA3800DD0A}")
//
//    ETW_EVENT(WmGenericImpl, id="0", name="WmGeneric", message="%1 %2(%3, %4)", opcode="WmGeneric", keywords="MessageHook")
//        (ETW_OUT(HexInt32) uint32_t hwnd,
//         WMs message,
//         ETW_OUT(HexInt32) uint32_t wParam,
//         ETW_OUT(HexInt32) uint32_t lParam);
//
//ETW_PROVIDER_END
//"""
    ] |> joinLines
    
(* 
Tests for various errors and the handling thereof: 
    Enum member w/o required prefix
    Non consecutive enum members
    Entities with illegal names (provider, task, event etc)
    Opcodes & keywords w/ illegal ids
    Illegal symbols
    Tasks beginning before the last is ended
    Double task end
    All in types
    All allowed out types
    All pre-defined types (remove some?)
    Messages with too few %params
    Messages with too many %params
    Messages w/ Unicode + crazy shit
    Multiple keywords on an event
    Try set multiple opcodes, levels etc
    User-defined task that clashes w/ the auto-defined manifest emission task
    Same for events & keywords
    Assert one provider per header
*)
