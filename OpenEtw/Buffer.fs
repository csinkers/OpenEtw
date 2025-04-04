namespace OpenEtw
open System
open SerdesNet

[<Flags>]
type BufferFlag =
    | Normal          = 0x00us
    | FlushMarker     = 0x01us
    | EventsLost      = 0x02us
    | BufferLost      = 0x04us
    | RtBackupCorrupt = 0x08us
    | RtBackup        = 0x10us
    | ProcIndex       = 0x20us
    | Compressed      = 0x40us

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module BufferFlag =
    let private valueLookup =
        [
            (BufferFlag.FlushMarker     , "FlushMarker")
            (BufferFlag.EventsLost      , "EventsLost")
            (BufferFlag.BufferLost      , "BufferLost")
            (BufferFlag.RtBackupCorrupt , "RtBackupCorrupt")
            (BufferFlag.RtBackup        , "RtBackup")
            (BufferFlag.ProcIndex       , "ProcIndex")
            (BufferFlag.Compressed      , "Compressed")
            (LanguagePrimitives.EnumOfValue 0x80us, "Unknown (0x80)")
        ]

    let fromUInt16 = LanguagePrimitives.EnumOfValue
    let info x =
        let str = 
            if x = BufferFlag.Normal then "Normal" 
            else
                valueLookup
                |> Seq.choose (fun (v, name) -> if (x &&& v <> BufferFlag.Normal) then Some name else None)
                |> String.concat " | "
        (uint16 x, str)

type BufferType =
    | Generic     = 0us
    | Rundown     = 1us
    | CtxSwap     = 2us
    | RefTime     = 3us
    | Header      = 4us
    | Batched     = 5us
    | EmptyMarker = 6us
    | DbgInfo     = 7us
    | Maximum     = 8us

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module BufferType =
    let fromUInt16 = LanguagePrimitives.EnumOfValue
    let info x =
        let str =
            match x with
            | BufferType.Generic     -> "Generic"
            | BufferType.Rundown     -> "Rundown"
            | BufferType.CtxSwap     -> "CtxSwap"
            | BufferType.RefTime     -> "RefTime"
            | BufferType.Header      -> "Header"
            | BufferType.Batched     -> "Batched"
            | BufferType.EmptyMarker -> "EmptyMarker"
            | BufferType.DbgInfo     -> "DbgInfo"
            | BufferType.Maximum     -> "Maximum"
            | x -> sprintf "Unknown (%d)" (uint16 x)
        (uint16 x, str)

type BufferState =
    | Free           = 0u
    | GeneralLogging = 1u
    | CSwitch        = 2u
    | Flush          = 3u
    | Maximum        = 4u

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module BufferState =
    let fromUInt32 = LanguagePrimitives.EnumOfValue
    let info x =
        let str = 
            match x with
            | BufferState.Free           -> "Free"
            | BufferState.GeneralLogging -> "GeneralLogging"
            | BufferState.CSwitch        -> "CSwitch"
            | BufferState.Flush          -> "Flush"
            | BufferState.Maximum        -> "Maximum"
            | x -> sprintf "Unknown (%d)" (uint32 x)
        (uint32 x, str)

(* https://www.geoffchappell.com/studies/windows/km/ntoskrnl/api/etw/tracelog/wmi_buffer_header.htm *)

type EtlBuffer() =
    static member headerSize = 0x48

    member val bufferSize     = 0x10000 with get, set
    member val savedOffset    = EtlBuffer.headerSize with get, set
    member val currentOffset  = EtlBuffer.headerSize with get, set
    member val refCount       = 0 with get, set
    member val timestamp      = 0L with get, set // Ticks after boot
    member val sequenceNumber = 1L with get, set
    member val unk20l         = 0L with get, set
    member val cpuId          = 0uy with get, set
    member val loggerId       = 0x10s with get, set
    member val bufferState    = BufferState.Flush with get, set
    member val offset         = EtlBuffer.headerSize with get, set
    member val bufferFlag     = BufferFlag.ProcIndex ||| BufferFlag.FlushMarker with get, set // 0x21us
    member val bufferType     = BufferType.Generic with get, set
    member val highPointer    = 0UL with get, set
    member val lowPointer     = 0UL with get, set
    member val events         = new System.Collections.Generic.List<BufferEvent>() with get

    member private x.Common (s : ISerdes) =
        x.bufferSize     <- s.Int32("bufferSize",     x.bufferSize)     // 0
        x.savedOffset    <- s.Int32("savedOffset",    x.savedOffset)    // 4
        x.currentOffset  <- s.Int32("currentOffset",  x.currentOffset)  // 8
        x.refCount       <- s.Int32("refCount",       x.refCount)       // c
        x.timestamp      <- s.Int64("timestamp",      x.timestamp)      // 10
        x.sequenceNumber <- s.Int64("sequenceNumber", x.sequenceNumber) // 18
        x.unk20l         <- s.Int64("unk20l",         x.unk20l)         // 20
        x.cpuId          <- s.UInt8("cpuId",          x.cpuId)          // 28
        s.UInt8("alignment", 0uy) |> ignore                             // 29
        x.loggerId       <- s.Int16("loggerId",       x.loggerId)       // 2a
        x.bufferState    <- s.EnumU32("bufferState", x.bufferState)     // 2c
        x.offset         <- s.Int32("offset", x.offset)                 // 30
        x.bufferFlag     <- s.EnumU16("bufferFlag", x.bufferFlag)       // 34
        x.bufferType     <- s.EnumU16("bufferType", x.bufferType)       // 36
        x.highPointer    <- s.UInt64("highPtr", x.highPointer)          // 38
        x.lowPointer     <- s.UInt64("lowPtr",  x.lowPointer)           // 40

    member x.Clone() = x.MemberwiseClone() :?> EtlBuffer
    member x.Serialize (s : ISerdes) =
        x.Common s
        s.Begin()
        x.events |> Seq.iteri (fun i e -> // 48
            s.NewLine()
            e.Serialize s
        )
        s.End()

        let paddingBytes = x.bufferSize - x.savedOffset
        if (paddingBytes > 0) then
            s.Pad(paddingBytes, 0xFFuy)

    static member Deserialize (s : ISerdes) =
        let x = EtlBuffer()
        let startOffset = s.Offset
        x.Common s
        let endOffset = startOffset + (int64 x.savedOffset)

        while(s.Offset < endOffset) do
            x.events.Add (BufferEvent.Deserialize s)

        let paddingBytes = x.bufferSize - x.savedOffset
        if (paddingBytes > 0) then
            s.Pad(paddingBytes, 0xFFuy)
        x

    static member BuildBuffers bufferSize events = seq {
        // TODO: Add checking to detect 32/64 bit mismatches?
        let mutable buffers = new System.Collections.Generic.Dictionary<uint8, EtlBuffer>()
        let mutable maxTimestamp = System.Int64.MinValue

        for (e:BufferEvent, cpuId : uint8) in events do
            if (buffers.ContainsKey cpuId |> not) then 
                let newBuffer = EtlBuffer()
                newBuffer.cpuId <- cpuId
                newBuffer.bufferSize <- bufferSize
                buffers.Add(cpuId, newBuffer)

            let buffer = buffers.[cpuId]

            if (buffer.offset + (Util.sizeWithPadding e.Size) > buffer.bufferSize + 1) then
                buffer.timestamp <- e.Timestamp
                yield buffer

                let newBuffer = EtlBuffer()
                newBuffer.events.Add e
                newBuffer.cpuId <- cpuId
                newBuffer.bufferSize <- bufferSize
                newBuffer.sequenceNumber <- buffer.sequenceNumber + 1L
                newBuffer.offset <- newBuffer.offset + Util.sizeWithPadding e.Size
                newBuffer.savedOffset <- newBuffer.offset
                buffers.[cpuId] <- newBuffer

            else
                buffer.events.Add e
                buffer.offset <- buffer.offset + Util.sizeWithPadding e.Size
                buffer.savedOffset <- buffer.offset

            if (e.Timestamp > maxTimestamp)  then
                maxTimestamp <- e.Timestamp

        for kvp in buffers do
            if (kvp.Value.events.Count > 0) then
                kvp.Value.timestamp <- maxTimestamp
                yield kvp.Value
    }
