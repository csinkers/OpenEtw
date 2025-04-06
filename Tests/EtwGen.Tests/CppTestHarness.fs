﻿module OpenEtw.Tests.CppTestHarness
open System
open OpenEtw

type EventPayload = Map<string, obj>
type CppBuffer = byte array

let coerce<'a> (x:obj) = match x with | :? 'a as t -> t | _ -> failwithf "Could not cast %A to %s" x (typeof<'a>.Name)
let build (provider:EtwProvider) (events : (EtwEvent * EventPayload) list) bitness (* TODO: Handle 32/64 bit *) =
    let perEvent f = events |> List.map f |> String.concat System.Environment.NewLine
    let never _ = None
    let hexVal =
        function
        | x when x >=  0 && x <=  9 -> char (x + int '0')
        | x when x >= 10 && x <= 15 -> char (x - 10 + int 'a')
        | _ -> failwith "Invalid hex conversion"

    let escapeString (s:string) = 
        seq {
            for c in s do
                match c with
                | '\a' -> yield '\\'; yield 'a'
                | '\b' -> yield '\\'; yield 'b'
                | '\f' -> yield '\\'; yield 'f'
                | '\n' -> yield '\\'; yield 'n'
                | '\r' -> yield '\\'; yield 'r'
                | '\t' -> yield '\\'; yield 't'
                | '\v' -> yield '\\'; yield 'v'
                | '\\' -> yield '\\'; yield '\\'
                | '"'  -> yield '\\'; yield '"'
                | x when x >= ' ' && x <= '~' -> yield x
                | x when (int x) < 256 -> 
                    yield '\\'; yield 'x'
                    yield (int x)/16 |> hexVal
                    yield (int x) % 16 |> hexVal

                | x when (int x) < 0x10000 -> 
                    yield '\\'; yield 'u'
                    yield ((int x) / 0x1000) % 0x10 |> hexVal
                    yield ((int x) / 0x100) % 0x10 |> hexVal
                    yield ((int x) / 0x10) % 0x10 |> hexVal
                    yield ((int x) / 0x1) % 0x10 |> hexVal
                | x ->
                    yield '\\'; yield 'U'
                    yield ((int x) / 0x10000000) % 0x10 |> hexVal
                    yield ((int x) / 0x1000000) % 0x10 |> hexVal
                    yield ((int x) / 0x100000) % 0x10 |> hexVal
                    yield ((int x) / 0x10000) % 0x10 |> hexVal
                    yield ((int x) / 0x1000) % 0x10 |> hexVal
                    yield ((int x) / 0x100) % 0x10 |> hexVal
                    yield ((int x) / 0x10) % 0x10 |> hexVal
                    yield ((int x) / 0x1) % 0x10 |> hexVal
        } |> Array.ofSeq |> System.String

    let valueFormatters = // Map from type name to tuple of preamble and value formatters.
        [
            "bool",               (never, (fun (_,x) -> if (coerce<bool> x) then "true" else "false"))
            "bool",               (never, (fun (_,x) -> if (coerce<bool> x) then "true" else "false"))
            "BOOL",               (never, (fun (_,x) -> if (coerce<bool> x) then "TRUE" else "FALSE"))

            "int",                (never, (fun (_,x) -> string <| coerce<int> x))
            "short",              (never, (fun (_,x) -> string <| coerce<int> x))
            "long",               (never, (fun (_,x) -> string <| coerce<int> x))
            "DWORD",              (never, (fun (_,x) -> string <| coerce<int> x))
            "int8_t",             (never, (fun (_,x) -> string <| coerce<int8> x))
            "char",               (never, (fun (_,x) -> string <| coerce<int8> x))
            "int16_t",            (never, (fun (_,x) -> string <| coerce<int16> x))
            "short",              (never, (fun (_,x) -> string <| coerce<int16> x))
            "int32_t",            (never, (fun (_,x) -> string <| coerce<int32> x))
            "int",                (never, (fun (_,x) -> string <| coerce<int32> x))
            "long",               (never, (fun (_,x) -> string <| coerce<int32> x))
            "int64_t",            (never, (fun (_,x) -> string <| coerce<int64> x))
            "__int64",            (never, (fun (_,x) -> string <| coerce<int64> x))
            "long long",          (never, (fun (_,x) -> string <| coerce<int64> x))

            "uint8_t",            (never, (fun (_,x) -> string <| coerce<uint8> x))
            "unsigned char",      (never, (fun (_,x) -> string <| coerce<uint8> x))
            "uint16_t",           (never, (fun (_,x) -> string <| coerce<uint16> x))
            "UINT16",             (never, (fun (_,x) -> string <| coerce<uint16> x))
            "unsigned short",     (never, (fun (_,x) -> string <| coerce<uint16> x))
            "uint32_t",           (never, (fun (_,x) -> string <| coerce<uint32> x))
            "unsigned int",       (never, (fun (_,x) -> string <| coerce<uint32> x))
            "unsigned long",      (never, (fun (_,x) -> string <| coerce<uint32> x))
            "DWORD",              (never, (fun (_,x) -> string <| coerce<uint32> x))
            "HRESULT",            (never, (fun (_,x) -> string <| coerce<uint32> x))
            "uint64_t",           (never, (fun (_,x) -> string <| coerce<uint64> x))
            "unsigned long long", (never, (fun (_,x) -> string <| coerce<uint64> x))

            "float",              (never, (fun (_,x) -> string <| coerce<single> x))
            "double",             (never, (fun (_,x) -> string <| coerce<float> x))

            "LPARAM",             (never, (fun (_,x) -> string <| coerce<int32> x)) // bitness (signed)
            "WPARAM",             (never, (fun (_,x) -> string <| coerce<uint32> x)) // bitness (unsigned)
            "void *",             (never, (fun (p,x) -> string <| sprintf "%sBuf" p))
            "BYTE *",             (never, (fun (p,x) -> string <| coerce<CppBuffer> x))
            "HANDLE",             (never, (fun (_,x) -> string <| coerce<uint32> x)) // bitness
            "LPSTR",
                (
                    (fun (p, x) -> Some (sprintf "LPSTR %sString = \"%s\";" p (escapeString <| coerce<string> x))), 
                    (fun (p,x) -> sprintf "%sString" p)
                )
            "LPCSTR",
                (
                    (fun (p, x) -> Some (sprintf "LPCSTR %sString = \"%s\";" p (escapeString <| coerce<string> x))), 
                    (fun (p,x) -> sprintf "%sString" p)
                )
            "char *",
                (
                    (fun (p, x) -> Some (sprintf "LPSTR %sString = \"%s\";" p (escapeString <| coerce<string> x))), 
                    (fun (p,x) -> sprintf "%sString" p)
                )
            "unsigned char *",    (never, (fun (p,x) -> string <| coerce<string> x)) // TODO: Check
            "wchar_t *",
                (
                    (fun (p, x) -> Some (sprintf "wchar_t *%sString = L\"%s\";" p (escapeString <| coerce<string> x))), 
                    (fun (p,x) -> sprintf "%sString" p)
                )
            "LPWSTR",
                (
                    (fun (p, x) -> Some (sprintf "LPWSTR %sString = L\"%s\";" p (escapeString <| coerce<string> x))), 
                    (fun (p,x) -> sprintf "%sString" p)
                )
            "LPCWSTR",
                (
                    (fun (p, x) -> Some (sprintf "LPCWSTR %sString = L\"%s\";" p (escapeString <| coerce<string> x))), 
                    (fun (p,x) -> sprintf "%sString" p)
                )
            "GUID *",             (never, (fun (p,x) -> string <| coerce<Guid> x))
            "FILETIME *",         (never, (fun (p,x) -> string <| coerce<DateTime> x))
            "SYSTEMTIME *",       (never, (fun (p,x) -> string <| coerce<DateTime> x))
            // "SID *",              (never, (fun (p,x) -> string <| coerce<> x))
        ] |> Map.ofList

    [
        (sprintf """#include "Provider.h"

void main()
{
    %s::Register();
        """ provider.className)

        (perEvent (fun (e, payload) ->
            let param name = Map.tryFind name payload
            let preamble, parameters =
                let paramFunc p = 
                    let preambleFormatter, valueFormatter = valueFormatters |> Map.find p.cppType
                    let maybeValue = param p.name
                    match maybeValue with
                    | Some value -> (preambleFormatter (p.name, value), valueFormatter (p.name, value))
                    | None -> failwithf "Could not find parameter %s in payload" p.name

                let formatted = e.parameters |> List.map paramFunc

                (
                    formatted |> List.choose fst |> String.concat Environment.NewLine, 
                    formatted |> List.map snd |> String.concat ", "
                )

            if (preamble <> "") then
                sprintf """    {
        %s
        %s::%s(%s);
    }"""                preamble provider.className e.cppName parameters
                
            else sprintf "    %s::%s(%s);" provider.className e.cppName parameters
        ))

        """}
        """
    ] |> String.concat System.Environment.NewLine
