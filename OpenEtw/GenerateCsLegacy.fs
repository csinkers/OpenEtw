module internal OpenEtw.GenerateCsLegacy
open OpenEtw.Util

let getId typeName collection selector key = 
    let pickFunction x =
        let entityId, entityName = selector x
        if (entityName = key) then Some entityId else None

    let result = collection |> List.tryPick pickFunction
    match result with
    | Some x -> x
    | None -> failwithf "Error: %s \"%s\" was not defined." typeName key

let csType =
    function
    | UnicodeString NullTerminated -> "string"
    | Int8 -> "Int8"
    | UInt8 -> "UInt8"
    | Int16 -> "Int16"
    | UInt16 -> "UInt16"
    | Int32 -> "Int32"
    | UInt32 -> "UInt32"
    | Int64 -> "Int64"
    | UInt64 -> "UInt64"
    | EtwType.Float -> "float"
    | EtwType.Double -> "double"
    | EtwType.Bool -> "bool"
    | EtwType.HexInt32 -> "Int32"
    | EtwType.HexInt64 -> "Int64"
    | x -> failwithf "%A parameters are not currently supported in C# providers" x

let paramDef useMaps (p : EtwEventParam) = 
    match useMaps, p.outType with
    | true, EtwOutType.Map name -> sprintf "%s %s" name p.name
    | _                         -> sprintf "%s %s" (csType p.inType) p.name

let buildCs (provider : EtwProvider) options =
    let perEvent f = provider.events |> List.map f |> String.concat System.Environment.NewLine
    let getIdForChannel = getId "Channel" provider.channels (fun c -> (c.id, c.name))
    let getIdForLevel   = getId "Level"   provider.levels   (fun l -> (l.id, l.name))
    let getIdForOpcode  = getId "Opcode"  provider.opcodes  (fun o -> (o.id, o.name))
    let getIdForTask    = getId "Task"    provider.tasks    (fun t -> (t.id, t.name))

    let getMaskForKeywords =
        function
        | [] -> 0UL
        | names ->
            let keywords = names |> List.map (fun n -> provider.keywords |> List.find (fun k -> k.name = n))
            List.fold (|||) 0UL (keywords |> List.map (fun k -> k.id))

    [
        (sprintf """using System;
using System.Diagnostics;
using System.Diagnostics.Eventing;
using System.Text;

namespace %s
{
    internal class %s : EventProvider
    {
        private static object _sync = new object();
        private static %s _instance;

        internal static %s GetInstance()
        {
            // https://en.wikipedia.org/wiki/Double-checked_locking#Usage_in_Microsoft_.NET_.28Visual_Basic.2C_C.23.29
            if (_instance == null)
            {
                lock (_sync)
                {
                    if (_instance == null)
                    {
                        _instance = new %s();
                    }
                }
            }

            return _instance;
        }

        %s() : base(new Guid("%s"))
        {
        }""" options.csNamespace 
             provider.className 
             provider.className 
             provider.className 
             provider.className 
             provider.className 
             (provider.guid.ToString()))

        (perEvent (fun e ->
            let paramValue (p:EtwEventParam) = sprintf ", %s" p.name

            sprintf """
        static EventDescriptor _ed%s = new EventDescriptor(%d, %d, %d, %d, %d, %d, %d);
        public void %s(%s)
        {
            WriteEvent(ref _ed%s%s);
        }"""
                (e.cppName)
                (e.id.Value)
                (e.version)
                ((e.channel |> Option.map getIdForChannel |?? Some 0uy).Value)
                (e.level    |> getIdForLevel)
                (e.opcode   |> Option.map getIdForOpcode  |?? 0uy)
                (e.task     |> Option.map getIdForTask    |?? 0us)
                (getMaskForKeywords e.keywords)

                e.cppName
                (e.parameters |> Seq.map (paramDef false) |> String.concat ", ")
                (e.cppName)
                (e.parameters |> Seq.map paramValue |> String.concat "")
        ))

        """    }
}
        """
    ] |> String.concat System.Environment.NewLine

let buildDecorator (provider : EtwProvider) (options : CsLegacyOptions) = 
    let perEvent f = provider.events |> List.map f |> String.concat System.Environment.NewLine
    let perTask f = provider.tasks |> List.map f |> String.concat System.Environment.NewLine
    let perMap f = provider.maps |> List.map f |> String.concat System.Environment.NewLine
    let perTaskEvent (t:EtwTask) f = 
        provider.events 
        |> List.where (
            function
            | { task = Some task } when task = t.name -> true
            | _ -> false)
        |> List.map f
        |> String.concat System.Environment.NewLine
        
    let interfaceName taskName = sprintf "I%sLog" taskName

    [
        (sprintf """using System;

namespace %s
{"""        options.csNamespace)

        // Interfaces for each task
        (perTask (fun t ->
            sprintf """    public partial interface %s
    {
%s
    }
"""             (interfaceName t.name)
                (perTaskEvent t (fun e -> 
                    sprintf "        void %s(%s);" 
                        e.cppName 
                        (e.parameters |> Seq.map (paramDef true) |> String.concat ", ")
                ))
        ))

        // Enums
        (perMap (fun map ->
            let perElem f = map.elements |> List.map f |> String.concat ("," + System.Environment.NewLine)
            sprintf """%s    public enum %s
    {
%s
    }
""" 
                (if map.mapType = BitMap then """    [Flags]
"""              else "")
                map.name
                (perElem (fun (name, n) -> sprintf "        %s = %d" name n))
        ))

        // The decorator
        (sprintf """    public partial class %sDecorator : %s
    {"""
            provider.className 
            (provider.tasks |> List.map (fun t -> interfaceName t.name) |> String.concat ", "))

        (perEvent (fun e ->
            let task = defaultArg e.task ""

            sprintf """        public void %s(%s)
        {
            %s.GetInstance().%s(%s);
        }
"""
                e.cppName
                (e.parameters |> Seq.map (paramDef true) |> String.concat ", ")
                provider.className
                (e.cppName)
                (e.parameters |> Seq.map (fun p -> 
                    match p.outType with
                    | Map _ -> sprintf "(UInt32)%s" p.name
                    | _ -> p.name
                ) |> String.concat ", ")
        ))

        """    }
}
        """
    ] |> String.concat System.Environment.NewLine

let forProvider (provider : EtwProvider) (options : CsLegacyOptions) =
    [
        Some (options.manifestFilename, GenerateManifest.forProvider provider)
        Some (options.csFilename, buildCs provider options)
        (options.decoratorFilename |> Option.map (fun x -> (x, buildDecorator provider options)))
    ] |> List.choose id
