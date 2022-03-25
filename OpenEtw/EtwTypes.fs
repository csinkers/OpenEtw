namespace OpenEtw
open System

type EtwChannelType =
    | Admin
    | Analytic
    | Debug
    | Operational
    override this.ToString() =
        match this with
        | Admin       -> "Admin"
        | Analytic    -> "Analytic"
        | Debug       -> "Debug"
        | Operational -> "Operational"

type EtwChannelIsolation =
    | Application
    | System
    override this.ToString() =
        match this with
        | Application -> "Application"
        | System      -> "System"

type EtwChannel =
    {
        id          : uint8 option
        name        : string
        symbol      : string
        chid        : string
        channelType : EtwChannelType
        isolation   : EtwChannelIsolation option
        message     : string option
        enabled     : bool
        implicit    : bool
    }
    static member defaultChannels =
        let defaultChannel = { id = Some 0uy; name = ""; symbol = ""; chid = ""; channelType = Debug; isolation = None; message = None; enabled = true; implicit = true }
        [
            { defaultChannel with id = Some 0x0uy; name = "TraceClassic"; chid = "TraceClassic" }
            { defaultChannel with id = Some 0x8uy; name = "System";       chid = "System" }
            { defaultChannel with id = Some 0x9uy; name = "Application";  chid = "Application" }
            { defaultChannel with id = Some 0xauy; name = "Security";     chid = "Security" }
            { defaultChannel with id = Some 0xbuy; name = "TraceLogging"; chid = "TraceLogging" } // TODO: Determine actual TraceLogging id
        ]

// TODO: Check these
type StandardLevel = // System.Diagnostics.Eventing.Reader.StandardEventLevel
    | LogAlways = 1uy
    | Critical = 2uy
    | Error = 3uy
    | Warning = 4uy
    | Informational = 5uy
    | Verbose = 6uy

type EtwLevel =
    {
        id       : uint8
        name     : string
        symbol   : string
        message  : string option
        implicit : bool
    }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module EtwLevel =
    let empty = {id = 0uy; name = ""; symbol = ""; message = None; implicit = true}
    let always        = { empty with id = uint8 StandardLevel.LogAlways;     name = "win:LogAlways" }
    let critical      = { empty with id = uint8 StandardLevel.Critical;      name = "win:Critical" }
    let error         = { empty with id = uint8 StandardLevel.Error;         name = "win:Error" }
    let warning       = { empty with id = uint8 StandardLevel.Warning;       name = "win:Warning" }
    let informational = { empty with id = uint8 StandardLevel.Informational; name = "win:Informational" }
    let verbose       = { empty with id = uint8 StandardLevel.Verbose;       name = "win:Verbose" }
    let defaultLevels =
        [
            always
            critical
            error
            warning
            informational
            verbose
        ]

// TODO: Check these
type StandardOpcode = // System.Diagnostics.Eventing.Reader.StandardEventOpcode
    | Info = 0uy
    | Start = 1uy
    | Stop = 2uy
    | DataCollectionStart = 3uy
    | DataCollectionStop = 4uy
    | Extension = 5uy
    | Reply = 6uy
    | Resume = 7uy
    | Suspend = 8uy
    | Send = 9uy
    | Receive = 10uy

type EtwOpcode =
    {
        id       : uint8
        name     : string
        symbol   : string
        message  : string option
        implicit : bool
    }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module EtwOpcode =
    let empty = { id = 0uy; name = ""; symbol = ""; message = None; implicit = true }
    let info      = { empty with name = "win:Info";      id = uint8 StandardOpcode.Info}
    let start     = { empty with name = "win:Start";     id = uint8 StandardOpcode.Start}
    let stop      = { empty with name = "win:Stop";      id = uint8 StandardOpcode.Stop}
    let dcStart   = { empty with name = "win:DcStart";   id = uint8 StandardOpcode.DataCollectionStart}
    let dcStop    = { empty with name = "win:DcStop";    id = uint8 StandardOpcode.DataCollectionStop}
    let extension = { empty with name = "win:Extension"; id = uint8 StandardOpcode.Extension}
    let reply     = { empty with name = "win:Reply";     id = uint8 StandardOpcode.Reply}
    let resume    = { empty with name = "win:Resume";    id = uint8 StandardOpcode.Resume}
    let suspend   = { empty with name = "win:Suspend";   id = uint8 StandardOpcode.Suspend}
    let send      = { empty with name = "win:Send";      id = uint8 StandardOpcode.Send}
    let receive   = { empty with name = "win:Receive";   id = uint8 StandardOpcode.Receive}
    let defaultOpcodes = [ info; start; stop; dcStart; dcStop; extension; reply; resume; suspend; send; receive ]

// TODO: Check these
type StandardKeywords = // System.Diagnostics.Eventing.Reader.StandardEventKeywords
    | EventLogClassic  = 0x01UL
    | CorrelationHint2 = 0x02UL
    | AuditSuccess     = 0x04UL
    | AuditFailure     = 0x08UL
    | Sqm              = 0x10UL
    | WdiDiagnostic    = 0x20UL
    | WdiContext       = 0x40UL
    | ResponseTime     = 0x80UL

type EtwKeyword =
    {
        id       : uint64
        name     : string
        symbol   : string
        message  : string option
        implicit : bool
    }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module EtwKeyword =
    let empty           = { id = 0UL; name = ""; symbol = ""; message = None; implicit = true }
    let eventLogClassic = { empty with name = "win:EventLogClassic"; id = uint64 StandardKeywords.EventLogClassic}
    let correlationHint = { empty with name = "win:CorrelationHint"; id = uint64 StandardKeywords.CorrelationHint2}
    let auditSuccess    = { empty with name = "win:AuditSuccess";    id = uint64 StandardKeywords.AuditSuccess }
    let auditFailure    = { empty with name = "win:AuditFailure";    id = uint64 StandardKeywords.AuditFailure}
    let sqm             = { empty with name = "win:SQM";             id = uint64 StandardKeywords.Sqm}
    let wdiDiag         = { empty with name = "win:WDIDiag";         id = uint64 StandardKeywords.WdiDiagnostic}
    let wdiContext      = { empty with name = "win:WDIContext";      id = uint64 StandardKeywords.WdiContext}
    let responseTime    = { empty with name = "win:ResponseTime";    id = uint64 StandardKeywords.ResponseTime}

    // EventSource keywords
    let session3 = { empty with name = "Session3"; symbol = "Session3"; message = Some "Session3"; id = 0x100000000000UL; implicit = false }
    let session2 = { empty with name = "Session2"; symbol = "Session2"; message = Some "Session2"; id = 0x200000000000UL; implicit = false }
    let session1 = { empty with name = "Session1"; symbol = "Session1"; message = Some "Session1"; id = 0x400000000000UL; implicit = false }
    let session0 = { empty with name = "Session0"; symbol = "Session0"; message = Some "Session0"; id = 0x800000000000UL; implicit = false }

    let defaultKeywords =
        [
            eventLogClassic 
            correlationHint 
            auditSuccess    
            auditFailure    
            sqm             
            wdiDiag         
            wdiContext      
            responseTime    

            // EventSource keywords
            session3 
            session2 
            session1 
            session0 
        ]

type EtwCount =
    | Single
    | Fixed of int
    | Counted of string
    override this.ToString() =
        match this with
        | Single -> "Single"
        | Fixed n -> sprintf "Fixed(%d)" n
        | Counted arg -> sprintf "Counted(%s)" arg

type EtwLength =
    | NullTerminated
    | Fixed of int
    | Counted of string
    override this.ToString() =
        match this with
        | NullTerminated -> "NullTerminated"
        | Fixed n -> sprintf "Fixed(%d)" n
        | Counted arg -> sprintf "Counted(%s)" arg

type EtwType =
    | UnicodeString of EtwLength
    | AnsiString of EtwLength
    | Int8
    | UInt8
    | Int16
    | UInt16
    | Int32
    | UInt32
    | Int64
    | UInt64
    | Float
    | Double
    | Bool
    | Binary of EtwLength
    | Guid
    | Pointer
    | FileTime
    | SystemTime
    | Sid
    | HexInt32
    | HexInt64
    | ActivityId
    | RelatedActivityId
    | Unresolved of string
    member this.info =
        match this with        // XML type          Canonical type         Header type      Length
        | UnicodeString len -> "win:UnicodeString", "const wchar_t *"   ,  "UnicodeString", Some len
        | AnsiString len    -> "win:AnsiString"   , "const char *"      ,  "AnsiString"   , Some len
        | Int8              -> "win:Int8"         , "const int8_t"      ,  "Int8"         , None
        | UInt8             -> "win:UInt8"        , "const uint8_t"     ,  "Int16"        , None
        | Int16             -> "win:Int16"        , "const int16_t"     ,  "Int32"        , None
        | UInt16            -> "win:UInt16"       , "const uint16_t"    ,  "Int64"        , None
        | Int32             -> "win:Int32"        , "const int32_t"     ,  "UInt8"        , None
        | UInt32            -> "win:UInt32"       , "const uint32_t"    ,  "UInt16"       , None
        | Int64             -> "win:Int64"        , "const int64_t"     ,  "UInt32"       , None
        | UInt64            -> "win:UInt64"       , "const uint64_t"    ,  "UInt64"       , None
        | Float             -> "win:Float"        , "const float"       ,  "Float"        , None
        | Double            -> "win:Double"       , "const double"      ,  "Double"       , None
        | Bool              -> "win:Boolean"      , "const bool"        ,  "Bool"         , None
        | Binary len        -> "win:Binary"       , "const BYTE *"      ,  "Binary"       , None
        | Guid              -> "win:GUID"         , "const GUID *"      ,  "Guid"         , None
        | Pointer           -> "win:Pointer"      , "const void *"      ,  "Pointer"      , None
        | FileTime          -> "win:FILETIME"     , "const FILETIME *"  ,  "FileTime"     , None
        | SystemTime        -> "win:SYSTEMTIME"   , "const SYSTEMTIME *",  "SystemTime"   , None
        | Sid               -> "win:SID"          , "const SID *"       ,  "Sid"          , None
        | HexInt32          -> "win:HexInt32"     , "const int32_t"     ,  "HexInt32"     , None
        | HexInt64          -> "win:HexInt64"     , "const int64_t"     ,  "HexInt64"     , None
        | ActivityId        -> "ActivityId"       , "ETW_ACTIVITYID"    ,  "ActivityId"   , None
        | RelatedActivityId -> "RelatedActivityId", "ETW_RELATED_ACTIVITYID", "RelatedActivityId", None
        | Unresolved typeName ->
            failwithf "%s \"%s\""
                ("This parameter's type could not be resolved. Either declare an " +
                "appropriate type mapping or add an ETW_IN override on the parameter. " +
                "The type name was ")
                typeName

    override this.ToString()    = this.info |> (fun (x, _, _, _) -> x)
    member this.CanonicalType() = this.info |> (fun (_, x, _, _) -> x)
    member this.HeaderType()    = this.info |> (fun (_, _, x, _) -> x)
    member this.Length()        = this.info |> (fun (_, _, _, x) -> x)
    static member parse =
        function
        | "UnicodeString" -> UnicodeString NullTerminated
        | "AnsiString"    -> AnsiString NullTerminated
        | "Int8"          -> Int8
        | "Int16"         -> Int16
        | "Int32"         -> Int32
        | "Int64"         -> Int64
        | "UInt8"         -> UInt8
        | "UInt16"        -> UInt16
        | "UInt32"        -> UInt32
        | "UInt64"        -> UInt64
        | "Float"         -> Float
        | "Double"        -> Double
        | "Bool"          -> Bool
        | "Binary"        -> Binary NullTerminated
        | "Guid"          -> Guid
        | "Pointer"       -> Pointer
        | "FileTime"      -> FileTime
        | "SystemTime"    -> SystemTime
        | "Sid"           -> Sid
        | "HexInt32"      -> HexInt32
        | "HexInt64"      -> HexInt64
        | "ActivityId"    -> ActivityId
        | "RelatedActivityId" -> RelatedActivityId
        | s -> failwithf "Unrecognised ETW in-type \"%s\"" s

type EtwOutType =
    | String
    | Xml
    | ReducedString
    | Byte
    | UnsignedByte
    | HexInt8
    | Short
    | UnsignedShort
    | Port
    | HexInt16
    | Int
    | HResult
    | UnsignedInt
    | Pid
    | Tid
    | IPv4
    | EtwTime
    | ErrorCode
    | Win32Error
    | NtStatus
    | HexInt32
    | Long
    | UnsignedLong
    | HexInt64
    | Float
    | Double
    | Bool
    | HexBinary
    | IPv6
    | SocketAddress
    | Guid
    | DateTime
    | DateTimeCultureInsensitive
    | Map of string
    | ActivityId
    | RelatedActivityId
    | Unresolved of string
    member this.info =
        match this with // OutParam   OutType                HeaderType
        | String        -> "outType", "xs:string"          , "String"
        | Xml           -> "outType", "win:Xml"            , "Xml"
        | ReducedString -> "outType", "trace:ReducedString", "ReducedString"
        | Byte          -> "outType", "xs:byte"            , "Byte"
        | UnsignedByte  -> "outType", "xs:unsignedByte"    , "UnsignedByte"
        | HexInt8       -> "outType", "win:HexInt8"        , "HexInt8"
        | Short         -> "outType", "xs:short"           , "Short"
        | UnsignedShort -> "outType", "xs:unsignedShort"   , "UnsignedShort"
        | Port          -> "outType", "win:Port"           , "Port"
        | HexInt16      -> "outType", "win:HexInt16"       , "HexInt16"
        | Int           -> "outType", "xs:int"             , "Int"
        | HResult       -> "outType", "win:HResult"        , "HResult"
        | UnsignedInt   -> "outType", "xs:unsignedInt"     , "UnsignedInt"
        | Pid           -> "outType", "win:PID"            , "Pid"
        | Tid           -> "outType", "win:TID"            , "Tid"
        | IPv4          -> "outType", "win:IPv4"           , "IPv4"
        | EtwTime       -> "outType", "win:ETWTIME"        , "EtwTime"
        | ErrorCode     -> "outType", "win:ErrorCode"      , "ErrorCode"
        | Win32Error    -> "outType", "win:Win32Error"     , "Win32Error"
        | NtStatus      -> "outType", "win:NTSTATUS"       , "NtStatus"
        | HexInt32      -> "outType", "win:HexInt32"       , "HexInt32"
        | Long          -> "outType", "xs:long"            , "Long"
        | UnsignedLong  -> "outType", "xs:unsignedLong"    , "UnsignedLong"
        | HexInt64      -> "outType", "win:HexInt64"       , "HexInt64"
        | Float         -> "outType", "xs:float"           , "Float"
        | Double        -> "outType", "xs:double"          , "Double"
        | Bool          -> "outType", "xs:boolean"         , "Bool"
        | HexBinary     -> "outType", "xs:hexBinary"       , "HexBinary"
        | IPv6          -> "outType", "win:IPv6"           , "IPv6"
        | SocketAddress -> "outType", "win:SocketAddress"  , "SocketAddress"
        | Guid          -> "outType", "xs:GUID"            , "Guid"
        | DateTime      -> "outType", "xs:dateTime"        , "DateTime"
        | DateTimeCultureInsensitive -> "outType", "win:DateTimeCultureInsensitive", "DateTimeCultureInsensitive"
        | Map str       -> "map"    , str                  , str
        | ActivityId    -> "special", "ActivityId"         , "ActivityId"
        | RelatedActivityId -> "special", "RelatedActivityId", "RelatedActivityId"
        | Unresolved s  ->
            failwithf "%s \"%s\""
                ("This parameter should have been resolved by the provider generation process," +
                " either declare an appropriate type mapping or add an ETW_OUT override on the parameter." +
                " The type name was ")
                s

    member this.MetaTypeName() = this.info |> (fun (x, _, _) -> x)
    override this.ToString()   = this.info |> (fun (_, x, _) -> x)
    member this.HeaderType()   = this.info |> (fun (_, _, x) -> x)
    static member parse s =
        match s with
        | "String"        -> String
        | "Xml"           -> Xml
        | "ReducedString" -> ReducedString
        | "Byte"          -> Byte
        | "UnsignedByte"  -> UnsignedByte
        | "HexInt8"       -> HexInt8
        | "Short"         -> Short
        | "UnsignedShort" -> UnsignedShort
        | "Port"          -> Port
        | "HexInt16"      -> HexInt16
        | "Int"           -> Int
        | "HResult"       -> HResult
        | "UnsignedInt"   -> UnsignedInt
        | "Pid"           -> Pid
        | "Tid"           -> Tid
        | "IPv4"          -> IPv4
        | "EtwTime"       -> EtwTime
        | "ErrorCode"     -> ErrorCode
        | "Win32Error"    -> Win32Error
        | "NtStatus"      -> NtStatus
        | "HexInt32"      -> HexInt32
        | "Long"          -> Long
        | "UnsignedLong"  -> UnsignedLong
        | "HexInt64"      -> HexInt64
        | "Float"         -> Float
        | "Double"        -> Double
        | "Bool"          -> Bool
        | "HexBinary"     -> HexBinary
        | "IPv6"          -> IPv6
        | "SocketAddress" -> SocketAddress
        | "Guid"          -> Guid
        | "DateTime"      -> DateTime
        | "DateTimeCultureInsensitive" -> DateTimeCultureInsensitive
        | "ActivityId"    -> ActivityId
        | "RelatedActivityId" -> RelatedActivityId
        | s -> failwithf "Unrecognised out type \"%s\"" s
    member out.IsTypePairAllowed =
        match out with
        | String        -> function | UnicodeString _ | AnsiString _ | Sid -> true |_ -> false
        | Xml           -> function | UnicodeString _ | AnsiString _ -> true |_ -> false
        | ReducedString -> function | UnicodeString _ | AnsiString _ -> true |_ -> false
        | Byte          -> (=) Int8
        | UnsignedByte  -> (=) UInt8
        | HexInt8       -> (=) UInt8
        | Short         -> (=) Int16
        | UnsignedShort -> (=) UInt16
        | Port          -> (=) UInt16
        | HexInt16      -> (=) UInt16
        | Int           -> (=) Int32
        | HResult       -> (=) Int32
        | UnsignedInt   -> (=) UInt32
        | Pid           -> (=) UInt32
        | Tid           -> (=) UInt32
        | IPv4          -> (=) UInt32
        | EtwTime       -> (fun x -> [ UInt32; UInt64 ] |> List.contains x)
        | ErrorCode     -> (fun x -> [ UInt32; EtwType.HexInt32 ] |> List.contains x)
        | Win32Error    -> (fun x -> [ UInt32; EtwType.HexInt32 ] |> List.contains x)
        | NtStatus      -> (fun x -> [ UInt32; EtwType.HexInt32 ] |> List.contains x)
        | HexInt32      -> (fun x -> [ UInt32; EtwType.HexInt32 ] |> List.contains x)
        | Long          -> (=) Int64
        | UnsignedLong  -> (=) UInt64
        | HexInt64      -> (fun x -> [ UInt64; Pointer; EtwType.HexInt64 ] |> List.contains x)
        | Float         -> (=) EtwType.Float
        | Double        -> (=) EtwType.Double
        | Bool          -> (=) EtwType.Bool
        | HexBinary     -> function |Binary _ -> true |_ -> false
        | IPv6          -> function |Binary _ -> true |_ -> false
        | SocketAddress -> function |Binary _ -> true |_ -> false
        | Guid          -> (=) EtwType.Guid
        | DateTime      -> (fun x -> [ FileTime; SystemTime ] |> List.contains x)
        | DateTimeCultureInsensitive -> (fun x -> [ FileTime; SystemTime ] |> List.contains x)
        | Map _         -> (=) Int32
        | Unresolved _  -> (fun _ -> false)
        | ActivityId    -> (=) EtwType.ActivityId
        | RelatedActivityId -> (=) EtwType.RelatedActivityId

type EtwMapType =
    | BitMap
    | ValueMap

type EtwMap =
    {
        name     : string
        mapType  : EtwMapType
        prefix   : string option
        elements : (string * int) list
    }

type EtwTask =
    {
        id      : uint16
        name    : string
        guid    : Guid option
        symbol  : string option
        message : string option
    }
    static member empty = { id = 0us; name = ""; guid = None; symbol = None; message = None }
    static member defaultTasks =
        [
            { EtwTask.empty with id = 65534us; name = "EventSourceMessage"; message = Some "EventSourceMessage" }
        ]

type EtwEventParam =
    {
        name       : string
        cppType    : string
        inType     : EtwType
        count      : EtwCount
        outType    : EtwOutType
    }
    static member isTemplateParameter p =
        match p.inType with
        | EtwType.ActivityId // Activity ids are set via parameters on EventEx and shouldn't appear in templates
        | EtwType.RelatedActivityId -> false
        | _ -> true

type EtwEvent = // A provider event
    {
        id         : uint16 option
        name       : string
        cppName    : string
        symbol     : string
        message    : string option
        version    : uint8
        task       : string option
        level      : string
        channel    : string option
        opcode     : string option
        keywords   : string list
        parameters : EtwEventParam list
    }
    static member empty =
        {
            id         = None
            name       = ""
            cppName    = ""
            symbol     = ""
            message    = None
            version    = 0uy
            task       = None
            level      = EtwLevel.verbose.name
            channel    = None
            opcode     = None
            keywords   = []
            parameters = []
        }

type EtwStrategy =
    | CppSelfDescribing // C++ provider that emits its own manifest in special events when traces are started / stopped.
    | CppManifested     // C++ provider that requires a resource dll to be registered via wevtutil in order to decode events.
    | CsLegacy          // C# provider using System.Diagnostics.Eventing.EventProvider, requires a native dll to store the manifest resources to allow decoding.
    | CsEventSource     // C# provider using System.Diagnostics.Tracing.EventSource.
    | NodeJs            // C++ provider exposed as nodejs module

type EtwProvider =
    {
        className  : string // Name of the static class that contains the logger methods
        name       : string // Name of the provider that will show up in WPA etc
        symbol     : string // Emitted in the provider, but doesn't currently affect the generated code. Should this take over className's job?
        guid       : Guid   // Can be used when creating an ETW session, or if using PerfView "*ProviderName" should also work
        resourceFilename : string option
        messageFilename : string option

        headers    : string list
        levels     : EtwLevel   list
        channels   : EtwChannel list
        opcodes    : EtwOpcode  list
        keywords   : EtwKeyword list
        maps       : EtwMap     list
        tasks      : EtwTask    list
        events     : EtwEvent   list
    }
    static member empty =
        {
            className  = ""
            name       = ""
            symbol     = ""
            guid       = Guid.Empty
            resourceFilename = None
            messageFilename = None

            headers    = []
            levels     = []
            opcodes    = []
            keywords   = []
            channels   = []
            maps       = []
            tasks      = []
            events     = []
        }

type CppSelfDescribingOptions =
    {
        etwGenComment : string
        insertDebugLogging : bool
        cppFilename : string
        headerName : string
    }
    static member defaults =
        {
            etwGenComment = ""
            insertDebugLogging = false
            cppFilename = ""
            headerName = ""
        }

type CsLegacyOptions =
    {
        csFilename : string
        csNamespace : string
        manifestFilename : string
        decoratorFilename : string option
    }

type NodeOptions =
    {
        usePrecompiledHeader : bool
        cppFilename : string
        headerName : string
    }
    static member defaults =
        {
            usePrecompiledHeader = true
            cppFilename = ""
            headerName = ""
        }
