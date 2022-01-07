module internal OpenEtw.ProcessTokenList

open OpenEtw.SourceHeaderParser

type TokenProcessorState =
    {
        provider : ProviderDeclaration option
        activeTask : string option
        activeVersion : uint8
        nextChannelId : uint8

        events   : EtwEvent list
        tasks    : EtwTask list
        maps     : EtwMap list
        types    : Map<string, EtwType * EtwOutType * string>
        channels : EtwChannel list
        levels   : EtwLevel list
        opcodes  : EtwOpcode list
        keywords : EtwKeyword list
    }
    static member empty =
        {
            provider = None
            activeVersion = 0uy
            activeTask    = None
            nextChannelId = 0x10uy

            events   = []
            tasks    = EtwTask.defaultTasks
            maps     = []
            types    = Map.empty
            channels = EtwChannel.defaultChannels
            levels   = EtwLevel.defaultLevels
            opcodes  = EtwOpcode.defaultOpcodes
            keywords = EtwKeyword.defaultKeywords
        }

let processTokens results =
    let finaliseProvider state = 
        let tasks = 
            state.tasks 
            |> Seq.sort 
            |> Seq.distinct 
            |> Seq.sortBy (fun t -> t.id)
            |> List.ofSeq

        let formatErrors entityName errors =
            if Seq.isEmpty errors then () else
            let combinedIds = errors |> String.concat ", "
            failwithf "Duplicate %s detected: %s" entityName combinedIds

        let duplicateCheck entityName predicate formatter =
            Seq.choose (fun x ->
                match predicate x with
                | Some i -> Some (i, x)
                | None -> None)
            >> Seq.groupBy fst
            >> Seq.choose (fun (key, x) ->
                if Seq.length x > 1 then 
                    Some ((string key) + ": " + (x |> Seq.map (snd >> formatter) |> String.concat ", "))
                else None)
            >> formatErrors entityName

        duplicateCheck "keyword id(s)"  (fun (k:EtwKeyword) -> Some k.id)   string state.keywords
        duplicateCheck "task number(s)" (fun (t:EtwTask)    -> Some t.id)   string tasks
        duplicateCheck "task name(s)"   (fun (t:EtwTask)    -> Some t.name) string tasks
        duplicateCheck "task symbol(s)" (fun (t:EtwTask)    -> t.symbol)    string tasks
        duplicateCheck "task guid(s)"   (fun (t:EtwTask)    -> t.guid)      string tasks
        duplicateCheck "event id(s)"    (fun (e:EtwEvent)   -> e.id)        string state.events
        duplicateCheck "opcode id(s)"   (fun (o:EtwOpcode)  -> Some o.id)   string state.opcodes
        duplicateCheck "opcode name(s)" (fun (o:EtwOpcode)  -> Some o.name) string state.opcodes

        if (state.provider.IsNone) then failwith "No ETW_PROVIDER declaration was found in the source file"
        let providerDeclaration = state.provider.Value

        let allocateEventIds events =
            let explicitIds = events |> List.choose (fun e -> e.id) |> Set.ofList
            let rec aux i =
                function
                | x::xs ->
                    match x.id with
                    | None ->
                        if (Set.contains i explicitIds) then aux (i+1us) (x::xs)
                        else {x with id = Some i}::(aux (i+1us) xs)
                    | Some _ -> x::(aux i xs)
                | [] -> []

            aux 0us events

        {
            className  = providerDeclaration.className
            name       = providerDeclaration.name
            symbol     = providerDeclaration.symbol
            guid       = providerDeclaration.guid
            resourceFilename = providerDeclaration.resourceFilename
            messageFilename  = providerDeclaration.messageFilename

            levels     = List.rev state.levels
            opcodes    = List.rev state.opcodes
            keywords   = List.rev state.keywords
            channels   = List.rev state.channels
            maps       = List.rev state.maps
            tasks      = tasks
            events     = state.events |> List.rev |> allocateEventIds
        }

    let resolveCustomTypes types (parameter : EtwEventParam) =
        match parameter.inType with
        | EtwType.Unresolved typeName ->
            match Map.tryFind typeName types with
            | Some (inType, outType, typeCast) -> 
                {parameter with
                    inType = inType
                    outType = outType
                }
            | None -> failwithf "Unknown type \"%s\" used in parameter \"%s\"" typeName parameter.name
        | _ -> parameter

    let processToken state =
        function
        | Provider p -> {state with provider = Some p}
        | TaskBegin t -> 
            match t.id with
            | 0us -> {state with activeTask = None}
            | _ ->
                {state with 
                    activeTask = Some t.name
                    tasks = t::state.tasks 
                }
        | TaskEnd -> {state with activeTask = None}
        | Version version   -> {state with activeVersion = version}
        | DeclareKeyword k  -> {state with keywords = k::state.keywords}
        | DeclareOpcode o   -> {state with opcodes  = o::state.opcodes}
        | DeclareType t     -> {state with types = Map.add t.typeName (t.inType, t.outType, t.typeCast) state.types}
        | DeclareChannel c  -> 
            let channel = {c with id = Some state.nextChannelId}
            {state with channels = channel::state.channels; nextChannelId = state.nextChannelId + 1uy}
        | DeclareLevel l    -> {state with levels = l::state.levels}
        | MapDefinition map -> 
            {state with 
                maps = map::state.maps
                types = Map.add map.name (EtwType.UInt32, EtwOutType.Map map.name, "") state.types
            }
        | Event e -> 
            let newEvent =
                {e with
                    name       = e.name
                    symbol     = e.symbol
                    version    = state.activeVersion
                    task       = state.activeTask
                    parameters = e.parameters |> List.map (resolveCustomTypes state.types)
                }

            { state with events = newEvent::state.events }
        | _ -> state

    let autogenerateOpcodes (state:TokenProcessorState) =
        if (not state.provider.Value.autoGenerateOpcodes) then state else

        let eventsWithoutOpcode = state.events |> List.where (fun e -> Option.isNone e.opcode) |> List.rev
        let existingOpcodeIds = state.opcodes |> List.map (fun o -> o.id)
        let availableIds = [0uy..255uy] |> List.except existingOpcodeIds

        let generatedOpcodes = 
            let rec aux (events:EtwEvent list) ids =
                match events, ids with
                | [], _ -> []
                | e::eventTail, i::idTail ->
                    {
                        EtwOpcode.id = i
                        name = e.name
                        symbol = e.name
                        message = None
                        implicit = false
                    }::(aux eventTail idTail)
                | _, [] -> 
                    failwith 
                        ("There are too many events without an explicit opcode to generate opcodes for each one. " +
                        "Either disable automatic opcode assignment, or explicitly specify more event opcodes so " +
                        "the total number of opcodes remains below the limit of 238.")

            aux eventsWithoutOpcode availableIds
            |> List.rev

        {state with 
            opcodes = List.concat [generatedOpcodes; state.opcodes]
            events = 
                state.events 
                |> List.map (fun e -> 
                    match e.opcode with
                    | None -> {e with opcode = Some e.name}
                    | _ -> e)
        }

    results 
    |> Seq.fold processToken TokenProcessorState.empty 
    |> autogenerateOpcodes
    |> finaliseProvider
