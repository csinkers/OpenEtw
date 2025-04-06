module internal OpenEtw.GenerateManifest
open OpenEtw.Util

let forProvider (provider : EtwProvider) =
    let newline = System.Environment.NewLine
    let mandatoryParam name value = $"{name}=\"{value}\""
    let optionalParam name value =
        match value with
        | Some str -> $"{name}=\"{str}\""
        | None -> ""

    let buildProvider () =
        sprintf """
      <provider %s>"""
            ([
                mandatoryParam "name" provider.name
                mandatoryParam "guid" <| provider.guid.ToString("B").ToUpper()
                mandatoryParam "symbol" provider.symbol
                mandatoryParam "resourceFileName" (defaultArg provider.resourceFilename provider.name)
                mandatoryParam "messageFileName" (defaultArg provider.messageFilename provider.name)
            ] |> String.concat " ")

    let buildEvents =
        let buildEvent (e : EtwEvent) =
            let getLevelId levelName = provider.levels |> List.pick (fun l -> if l.name = levelName then Some l.id else None)
            sprintf """          <event %s>
          </event>"""
                ([
                    mandatoryParam "symbol" e.symbol
                    mandatoryParam "value" <| string e.id.Value
                    mandatoryParam "version" <| string e.version
                    optionalParam "channel" <| e.channel
                    mandatoryParam "level" <| e.level
                    optionalParam "task" e.task
                    optionalParam "opcode" <| e.opcode
                    optionalParam "template" (if (e.parameters.IsEmpty) then None else Some e.name)
                    optionalParam "keywords"
                        <|  match e.keywords with
                            | [] -> None
                            | _ -> e.keywords |> String.concat " " |> (fun s -> s + " ") |> Some
                    optionalParam "message" <| (e.message |> Option.map (fun _ -> sprintf "$(string.%s.event.%d.message)" provider.name e.id.Value))
                ] |> List.where ((<>) "") |> String.concat " ")

        function
        | [] -> ""
        | events ->
            sprintf  """
        <events>
%s
        </events>""" (String.concat newline (events |> Seq.map buildEvent))

    let buildOpcodes =
        let buildOpcode (opcode : EtwOpcode) =
            sprintf """          <opcode %s>
          </opcode>"""
                ([
                    mandatoryParam "name" opcode.name
                    mandatoryParam "symbol" opcode.symbol
                    mandatoryParam "value" (string opcode.id)
                    optionalParam "message" (opcode.message |> Option.map (fun _ -> $"$(string.{provider.name}.opcode.{opcode.symbol}.message)"))
                ] |> List.where ((<>) "") |> String.concat " ")

        function
        | [] -> ""
        | opcodes ->
            $"""
        <opcodes>
{opcodes |> Seq.map buildOpcode |> String.concat newline}
        </opcodes>"""

    let buildTasks =
        let buildTask (task : EtwTask) =
            sprintf """          <task %s>
          </task>"""
                ([
                    mandatoryParam "name" task.name
                    mandatoryParam "value" (string task.id)
                    optionalParam "symbol" task.symbol
                    optionalParam "eventGUID" (task.guid |> Option.map (fun g -> g.ToString("B").ToUpper()))
                    optionalParam "message" (task.message |> Option.map (fun _ -> sprintf "$(string.%s.task.%s.message)" provider.name (task.symbol |?? task.name)))
                ] |> List.where ((<>) "") |> String.concat " ")

        function
        | [] -> ""
        | tasks ->
            $"""
        <tasks>
{String.concat newline (tasks |> Seq.map buildTask)}
        </tasks>"""

    let buildKeywords =
        let buildKeyword (k:EtwKeyword) =
            sprintf """
          <keyword %s>
          </keyword>"""
                ([
                    mandatoryParam "name" k.name
                    mandatoryParam "symbol" k.symbol
                    mandatoryParam "mask" (sprintf "0x%x" k.id)
                    optionalParam "message" (k.message |> Option.map (fun _ ->
                                                 sprintf "$(string.%s.Keyword.%s.message)"
                                                     provider.name
                                                     k.symbol))
                ] |> List.where ((<>) "") |> String.concat " ")
        function
        | [] -> ""
        | keywords ->
            let innerText =
                keywords
                |> Seq.sortBy (fun (k:EtwKeyword) -> k.id)
                |> Seq.map buildKeyword
                |> String.concat ""
            $"""
        <keywords>{innerText}
        </keywords>
        """

    let buildMaps =
        let buildMap map =
            let buildMapElement (name, num) =
                match map.mapType with
                | BitMap ->
                    $"""
            <map value="0x{num:x}" message="$(string.{provider.name}.map.{map.name}.{num}.message)" />"""
//                    sprintf """
//            <map value="0x%x" message="$(string.%s.map.%s.%d.message)">
//            </map>"""     num provider.name map.name num
                | ValueMap ->
                    sprintf """
            <map value="%d" message="$(string.%s.map.%s.%d.message)" />"""     num provider.name map.name num
//                    sprintf """
//            <map value="%d" message="$(string.%s.map.%s.%d.message)">
//            </map>"""     num provider.name map.name num

            let mapTypeName =
                match map.mapType with
                | BitMap -> "bitMap"
                | ValueMap -> "valueMap"

            sprintf """
          <%s name="%s">%s
          </%s>"""
                mapTypeName
                map.name
                (map.elements |> Seq.map buildMapElement |> String.concat "")
                mapTypeName

        function
        | [] -> ""
        | maps ->
            sprintf """<maps>%s
        </maps>
        """     (String.concat "" (maps |> Seq.map buildMap))

    let buildTemplates =
        let buildTemplate (e : EtwEvent) =
            let buildField (field : EtwEventParam) =
                let paramName, outType = field.outType.MetaTypeName(), field.outType.ToString()
                sprintf """
            <data %s>
            </data>"""
                    ([
                        mandatoryParam "name" field.name
                        mandatoryParam "inType" (field.inType.ToString())
                        mandatoryParam paramName outType

                        optionalParam "count"
                            (match field.count with
                            | EtwCount.Single -> None
                            | EtwCount.Fixed num -> Some (string num)
                            | EtwCount.Counted paramName -> Some paramName)

                        optionalParam "length"
                            (match field.inType with
                            | UnicodeString l
                            | AnsiString l
                            | Binary l ->
                                match l with
                                | NullTerminated -> None
                                | EtwLength.Fixed num -> Some (string num)
                                | EtwLength.Counted paramName -> Some paramName
                            | _ -> None)

                    ] |> List.where ((<>) "") |> String.concat " ")

            sprintf """
          <template tid="%s">%s
          </template>"""
                e.name
                (e.parameters |> Seq.where EtwEventParam.isTemplateParameter |> Seq.map buildField |> String.concat "")
        function
        | [] -> ""
        | events ->
            sprintf """
        <templates>%s
        </templates>"""     (String.concat "" (events |> Seq.where (fun e -> not e.parameters.IsEmpty) |> Seq.map buildTemplate))

    let buildChannels =
        let buildChannel (c : EtwChannel) =
            match c.implicit with
            | false ->
                sprintf """          <channel %s>
          </channel>""" ([
                            mandatoryParam "name" c.name
                            mandatoryParam "chid" c.chid
                            mandatoryParam "symbol" c.symbol
                            mandatoryParam "type" (c.channelType.ToString())
                            mandatoryParam "enabled" (if c.enabled then "true" else "false")
                            optionalParam "isolation" (c.isolation |> Option.map (fun i -> i.ToString()))
                            optionalParam "message" (c.message |> Option.map (fun _ -> sprintf "$(string.%s.channel.%s.message)" provider.name c.symbol))
                        ] |> List.where ((<>) "") |> String.concat " ")
            | true ->
                sprintf
                    "          <importChannel %s/>"
                    ([
                        mandatoryParam "name" c.name
                        mandatoryParam "chid" c.chid
                    ] |> List.where ((<>) "") |> String.concat " ")
        function
        | [] -> ""
        | channels ->
            sprintf """
        <channels>
%s
        </channels>""" (String.concat newline (channels |> Seq.map buildChannel))

    let buildLevels =
        let buildLevel (l : EtwLevel) =
            sprintf """<level %s>
          </level>
          """
                  ([
                    mandatoryParam "name" l.name
                    mandatoryParam "symbol" l.symbol
                    mandatoryParam "value" (string l.id)
                    optionalParam "message" (l.message |> Option.map (fun _ -> sprintf "$(string.%s.level.%s.message)" provider.name l.symbol))
                  ] |> List.where ((<>) "") |> String.concat " ")

        function
        | [] -> ""
        | levels ->
            sprintf """<levels>
          %s</levels>
        """         (String.concat "" (levels |> Seq.map buildLevel))

    let buildStringTable () =
        let buildString entityName identifier =
            function
            | None -> ""
            | Some message ->
                $"""
        <string id="{provider.name}.{entityName}.{identifier}.message" value="{message}">
        </string>"""

        let buildEventString   (e : EtwEvent)   = buildString "event"   (string e.id.Value) e.message
        let buildTaskString    (t : EtwTask)    = buildString "task"    (t.symbol |?? t.name) t.message
        let buildChannelString (c : EtwChannel) = buildString "channel" c.symbol          c.message
        let buildLevelString   (l : EtwLevel)   = buildString "level"   l.symbol          l.message
        let buildOpcodeString  (o : EtwOpcode)  = buildString "opcode"  o.symbol          o.message
        let buildKeywordString (k : EtwKeyword) = buildString "Keyword" k.symbol          k.message

        let buildMapString (map:EtwMap) =
            let inner (name : string, num) =
                let message =
                    match map.prefix with
                    | Some p ->
                        match map.name.IndexOf(p) with
                        | -1 -> name
                        | x -> name.Substring(x)
                    | None -> name
                $"""
        <string id="{provider.name}.map.{map.name}.{num}.message" value="{message}">
        </string>"""

            map.elements
            |> Seq.map inner
            |> String.concat ""

        [
            provider.tasks  |> Seq.map buildTaskString
            (provider.opcodes  |> Seq.where (fun o -> not o.implicit) |> Seq.map buildOpcodeString)
            provider.maps |> Seq.map buildMapString
            (provider.levels   |> Seq.where (fun l -> not l.implicit) |> Seq.map buildLevelString)
            provider.events |> Seq.map buildEventString
            (provider.channels |> Seq.where (fun c -> not c.implicit) |> Seq.map buildChannelString)
            (provider.keywords |> Seq.where (fun k -> not k.implicit) |> Seq.map buildKeywordString)
        ] |> Seq.concat |> String.concat ""

    sprintf """<instrumentationManifest xmlns="http://schemas.microsoft.com/win/2004/08/events">
  <instrumentation xmlns:xs="http://www.w3.org/2001/XMLSchema" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:win="http://manifests.microsoft.com/win/2004/08/windows/events">
    <events xmlns="http://schemas.microsoft.com/win/2004/08/events">%s%s%s%s%s%s%s%s%s
      </provider>
    </events>
  </instrumentation>
  <localization>
    <resources culture="en-US">
      <stringTable>%s
      </stringTable>
    </resources>
  </localization>
</instrumentationManifest>"""
        (buildProvider ())
        (buildEvents provider.events)
        (buildLevels (provider.levels |> List.where (fun l -> not l.implicit)))
        (buildTasks provider.tasks)
        (buildOpcodes (provider.opcodes|> List.where (fun l -> not l.implicit)))
        (buildChannels provider.channels) // No where-clause as implicit channels still require an import
        (buildKeywords (provider.keywords|> List.where (fun l -> not l.implicit)))
        (buildMaps provider.maps)
        (buildTemplates provider.events)
        (buildStringTable ())
