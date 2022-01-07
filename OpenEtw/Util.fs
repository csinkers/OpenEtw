module OpenEtw.Util
open System
open System.Security.Cryptography
open System.Text

[<System.Runtime.InteropServices.DllImport("kernel32.dll")>]
extern uint32 GetCurrentThreadId();

// Generates a GUID via hashing to be used w/ PerfView's EventSource support
// (e.g. Using *ProviderName instead of the full GUID or registering a provider)
let providerToGuid (name : string) =
    let namespaceBytes =
        [|
            0x48uy; 0x2Cuy; 0x2Duy; 0xB2uy; 0xC3uy; 0x90uy; 0x47uy; 0xC8uy;
            0x87uy; 0xF8uy; 0x1Auy; 0x15uy; 0xBFuy; 0xC1uy; 0x30uy; 0xFBuy
        |]

    use sha1 = SHA1.Create()
    let nameBytes = Encoding.BigEndianUnicode.GetBytes(name.ToUpperInvariant())
    sha1.TransformBlock(namespaceBytes, 0, namespaceBytes.Length, null, 0) |> ignore
    sha1.TransformFinalBlock(nameBytes, 0, nameBytes.Length) |> ignore
    let guidBytes = Array.init 16 (fun _ -> 0uy)
    Buffer.BlockCopy(sha1.Hash, 0, guidBytes, 0, 16)

    // Guid = Hash[0..15], with Hash[7] tweaked according to RFC 4122
    guidBytes.[7] <- ((guidBytes.[7] &&& 0x0Fuy) ||| 0x50uy)
    new Guid(guidBytes)

let (|??) a = // Like the C# ?? operator, but for option types.
    match a with
    | Some x -> (fun _ -> x)
    | None -> id

let (|Prefix|_|) (p:string) (s:string) =
    if (s.StartsWith p) then Some (s.Substring <| p.Length)
    else None

(*
module Seq =
  let groupAdjacent groupingFunction (input : seq<_>) = seq {
    use en = input.GetEnumerator()
    let running = ref true

    let rec group() = seq {
        let prev = en.Current
        if en.MoveNext() then
          if (groupingFunction (prev, en.Current)) then
              yield prev
              yield! group()
          else
              yield prev
        else
            yield prev
            running := false }

    if en.MoveNext() then
      while running.Value do
        yield group() }
*)

let etlEpoch = System.DateTime(1601, 1, 1)
let toEtlAbsoluteTicks (dt:System.DateTime) = dt.Ticks - etlEpoch.Ticks
let toEtlBootTicks (bootTime:System.DateTime) (dt:System.DateTime) = dt.Ticks - bootTime.Ticks
let fromEtlAbsoluteTicks ticks = System.DateTime(ticks + etlEpoch.Ticks)
let fromEtlBootTicks (bootTime:System.DateTime) ticks = System.DateTime(bootTime.Ticks + ticks)
let sizeWithPadding x = (8 * ((x + 7) / 8))
let paddingBytes x = (sizeWithPadding x) - x

let paddedString len (s:string) =
    let sBytes = System.Text.Encoding.Unicode.GetBytes(s)
    assert (sBytes.Length + 2 <= len)
    [
        sBytes
        (Array.create (len - sBytes.Length) 0uy)
    ] |> Array.concat

let hexStringTable = 
    [|
        "00"; "01"; "02"; "03"; "04"; "05"; "06"; "07"; "08"; "09"; "0A"; "0B"; "0C"; "0D"; "0E"; "0F"
        "10"; "11"; "12"; "13"; "14"; "15"; "16"; "17"; "18"; "19"; "1A"; "1B"; "1C"; "1D"; "1E"; "1F"
        "20"; "21"; "22"; "23"; "24"; "25"; "26"; "27"; "28"; "29"; "2A"; "2B"; "2C"; "2D"; "2E"; "2F"
        "30"; "31"; "32"; "33"; "34"; "35"; "36"; "37"; "38"; "39"; "3A"; "3B"; "3C"; "3D"; "3E"; "3F"
        "40"; "41"; "42"; "43"; "44"; "45"; "46"; "47"; "48"; "49"; "4A"; "4B"; "4C"; "4D"; "4E"; "4F"
        "50"; "51"; "52"; "53"; "54"; "55"; "56"; "57"; "58"; "59"; "5A"; "5B"; "5C"; "5D"; "5E"; "5F"
        "60"; "61"; "62"; "63"; "64"; "65"; "66"; "67"; "68"; "69"; "6A"; "6B"; "6C"; "6D"; "6E"; "6F"
        "70"; "71"; "72"; "73"; "74"; "75"; "76"; "77"; "78"; "79"; "7A"; "7B"; "7C"; "7D"; "7E"; "7F"
        "80"; "81"; "82"; "83"; "84"; "85"; "86"; "87"; "88"; "89"; "8A"; "8B"; "8C"; "8D"; "8E"; "8F"
        "90"; "91"; "92"; "93"; "94"; "95"; "96"; "97"; "98"; "99"; "9A"; "9B"; "9C"; "9D"; "9E"; "9F"
        "A0"; "A1"; "A2"; "A3"; "A4"; "A5"; "A6"; "A7"; "A8"; "A9"; "AA"; "AB"; "AC"; "AD"; "AE"; "AF"
        "B0"; "B1"; "B2"; "B3"; "B4"; "B5"; "B6"; "B7"; "B8"; "B9"; "BA"; "BB"; "BC"; "BD"; "BE"; "BF"
        "C0"; "C1"; "C2"; "C3"; "C4"; "C5"; "C6"; "C7"; "C8"; "C9"; "CA"; "CB"; "CC"; "CD"; "CE"; "CF"
        "D0"; "D1"; "D2"; "D3"; "D4"; "D5"; "D6"; "D7"; "D8"; "D9"; "DA"; "DB"; "DC"; "DD"; "DE"; "DF"
        "E0"; "E1"; "E2"; "E3"; "E4"; "E5"; "E6"; "E7"; "E8"; "E9"; "EA"; "EB"; "EC"; "ED"; "EE"; "EF"
        "F0"; "F1"; "F2"; "F3"; "F4"; "F5"; "F6"; "F7"; "F8"; "F9"; "FA"; "FB"; "FC"; "FD"; "FE"; "FF"
    |]

let convertToHex (bytes : byte array) =
    let result = System.Text.StringBuilder(bytes.Length * 2);
    for b in bytes do
        result.Append (hexStringTable.[int b]) |> ignore
    result.ToString()

type SerializerMode =
    | Reading
    | Writing of bool // true if annotations are being written

let readValue f name defValue =
    let mutable temp = defValue
    f name (fun () -> defValue) (fun v -> temp <- v)
    temp

let writeValue f name value = f name (fun () -> value) ignore

type ISerializer =
    abstract member Mode         : SerializerMode with get
    abstract member Offset       : int64 with get // For recording offsets to be overwritten later
    abstract member Comment      : string -> unit // Only affects annotating writers
    abstract member Indent       : unit -> unit   // Only affects annotating writers
    abstract member Unindent     : unit -> unit   // Only affects annotating writers
    abstract member NewLine      : unit -> unit   // Only affects annotating writers
    abstract member Seek         : int64 -> unit  // For overwriting pre-recorded offsets

    abstract member Int8         : string -> (unit -> int8)   -> (int8 -> unit)   -> unit
    abstract member Int16        : string -> (unit -> int16)  -> (int16 -> unit)  -> unit
    abstract member Int32        : string -> (unit -> int32)  -> (int32 -> unit)  -> unit
    abstract member Int64        : string -> (unit -> int64)  -> (int64 -> unit)  -> unit
    abstract member UInt8        : string -> (unit -> uint8)  -> (uint8 -> unit)  -> unit
    abstract member UInt16       : string -> (unit -> uint16) -> (uint16 -> unit) -> unit
    abstract member UInt32       : string -> (unit -> uint32) -> (uint32 -> unit) -> unit
    abstract member UInt64       : string -> (unit -> uint64) -> (uint64 -> unit) -> unit

    abstract member Guid         : string -> (unit -> System.Guid) -> (System.Guid -> unit) -> unit
    abstract member ByteArray    : string -> (unit -> byte array) -> (byte array -> unit) -> int -> unit
    abstract member ByteArrayHex : string -> (unit -> byte array) -> (byte array -> unit) -> int -> unit
    abstract member ByteArray2   : string -> (unit -> byte array) -> (byte array -> unit) -> int -> string -> unit
    abstract member NullTerminatedString : string -> (unit -> string) -> (string -> unit) -> unit
    abstract member FixedLengthString : string -> (unit -> string) -> (string -> unit) -> int -> unit
    abstract member RepeatU8     : string -> uint8 -> int -> unit // Either writes a block of padding or verifies the consistency of one while reading
    abstract member Meta         : string -> (ISerializer -> unit) -> (ISerializer -> unit) -> unit // name serializer deserializer

    abstract member EnumU8  : string -> (unit -> 'a) -> (uint8  -> unit) -> ('a -> uint8  * string) -> unit
    abstract member EnumU16 : string -> (unit -> 'a) -> (uint16 -> unit) -> ('a -> uint16 * string) -> unit
    abstract member EnumU32 : string -> (unit -> 'a) -> (uint32 -> unit) -> ('a -> uint32 * string) -> unit

type AnnotatedFormatWriter(tw : System.IO.TextWriter) =
    let mutable offset = 0L
    let mutable indent = 0
    let doIndent () = tw.Write(System.String(' ', indent))

    interface ISerializer with
        member x.Mode with get () = Writing true
        member x.Comment (msg : string) = doIndent(); tw.WriteLine("// {0}", msg)
        member x.Indent() = indent <- indent + 4
        member x.Unindent() = indent <- indent - 4
        member x.NewLine() = tw.WriteLine()
        member x.Offset with get () = offset
        member x.Seek newOffset = tw.WriteLine("Seek to {0} for overwrite", newOffset); offset <- newOffset

        member x.Int8 name getter setter = 
            doIndent()
            tw.WriteLine("{0:X} {1} = {2} (0x{2:X} y)", offset, name, getter())
            offset <- offset + 1L
        member x.Int16  name getter setter = 
            doIndent()
            tw.WriteLine("{0:X} {1} = {2} (0x{2:X} s)", offset, name, getter())
            offset <- offset + 2L
        member x.Int32  name getter setter = 
            doIndent()
            tw.WriteLine("{0:X} {1} = {2} (0x{2:X})", offset, name, getter())
            offset <- offset + 4L
        member x.Int64  name getter setter = 
            doIndent()
            tw.WriteLine("{0:X} {1} = {2} (0x{2:X} L)", offset, name, getter())
            offset <- offset + 8L

        member x.UInt8  name getter setter = 
            doIndent()
            tw.WriteLine("{0:X} {1} = {2} (0x{2:X} uy)", offset, name, getter())
            offset <- offset + 1L
        member x.UInt16 name getter setter = 
            doIndent()
            tw.WriteLine("{0:X} {1} = {2} (0x{2:X} us)", offset, name, getter())
            offset <- offset + 2L
        member x.UInt32 name getter setter = 
            doIndent()
            tw.WriteLine("{0:X} {1} = {2} (0x{2:X} u)", offset, name, getter())
            offset <- offset + 4L
        member x.UInt64 name getter setter =
            let v = getter()
            doIndent()
            tw.WriteLine("{0:X} {1} = 0x{2:X}`{3:X8} UL ({4})", offset, name, (v &&& 0xffffffff00000000UL) >>> 32, v &&& 0xffffffffUL, v)
            offset <- offset + 8L

        member x.EnumU8 name getter setter infoFunc = 
            let v = getter()
            let value, label = infoFunc v
            doIndent()
            tw.WriteLine("{0:X} {1} = {2} (0x{2:X} uy) // {3}", offset, name, value, label)
            offset <- offset + 1L

        member x.EnumU16 name getter setter infoFunc = 
            let v = getter()
            let value, label = infoFunc v
            doIndent()
            tw.WriteLine("{0:X} {1} = {2} (0x{2:X} us) // {3}", offset, name, value, label)
            offset <- offset + 2L

        member x.EnumU32 name getter setter infoFunc = 
            let v = getter()
            let value, label = infoFunc v
            doIndent()
            tw.WriteLine("{0:X} {1} = {2} (0x{2:X} u) // {3}", offset, name, value, label)
            offset <- offset + 4L

        member x.Guid name getter setter =
            let v = getter()
            doIndent()
            tw.WriteLine("{0:X} {1} = {2}", offset, name, v.ToString("B"))
            offset <- offset + 16L

        member x.ByteArray name getter setter n =
            let v = getter()
            doIndent()
            tw.WriteLine("{0:X} {1} = {2}", offset, name, convertToHex v)
            offset <- offset + (int64 v.Length)

        member x.ByteArray2 name getter setter n comment =
            let v = getter()
            doIndent()
            tw.WriteLine("{0:X} {1} = {2}", offset, name, comment)
            offset <- offset + (int64 v.Length)

        member x.ByteArrayHex name getter setter n = 
            let v = getter()
            doIndent()
            tw.Write("{0:X} {1} = ", offset, name)
            
            (x :> ISerializer).Indent()
            let mutable payloadOffset = 0
            let sb = System.Text.StringBuilder(16)
            for b in v do
                if (payloadOffset % 16 = 0) then
                    tw.Write(' ')
                    tw.Write(sb.ToString())
                    sb.Clear() |> ignore
                    tw.WriteLine()
                    doIndent()
                    tw.Write("{0:X4}: ", payloadOffset)
                else if (payloadOffset % 8 = 0) then tw.Write('-')
                else if (payloadOffset % 2 = 0) then tw.Write(' ')

                tw.Write("{0:X2}", b)

                if(b >= (byte ' ') && b <= 0x7euy) then sb.Append(System.Convert.ToChar(b)) |> ignore
                else sb.Append('.') |> ignore

                payloadOffset <- payloadOffset + 1

            if(sb.Length > 0) then 
                let missingChars = 16 - sb.Length
                let spaceCount = (missingChars * 2) + (missingChars / 2) + 1
                tw.Write(Array.create spaceCount ' ')
                tw.Write(sb.ToString())

            tw.WriteLine()
            (x :> ISerializer).Unindent()
            offset <- offset + (int64 v.Length)

        member x.NullTerminatedString name getter setter =
            let v = getter()
            doIndent()
            tw.Write("{0:X} {1} = {2}", offset, name, v)

            let bytes = System.Text.Encoding.Unicode.GetBytes(v)
            offset <- offset + int64 (bytes.Length + 2) // add 2 bytes for the null terminator

        member x.FixedLengthString name getter setter length =
            let v = getter()
            doIndent()
            tw.Write("{0:X} {1} = {2}", offset, name, v)

            let bytes = System.Text.Encoding.Unicode.GetBytes(v)
            if (bytes.Length > length + 2) then failwith "Tried to write overlength string"

            offset <- offset + int64 length // add 2 bytes for the null terminator

        member x.RepeatU8 name v length = 
            doIndent()
            tw.WriteLine(
                "{0:X} {1} = [{2} bytes (0x{2:X}) of 0x{3:X}]", 
                offset, 
                name, 
                length,
                v
            )
            offset <- offset + (int64 length)

        member x.Meta name serializer deserializer =
            indent <- indent + 4
            doIndent()
            tw.WriteLine("// {0}", name)
            serializer x
            indent <- indent - 4

type GenericBinaryWriter(bw : System.IO.BinaryWriter) =
    let mutable offset = 0L

    interface ISerializer with
        member x.Mode with get () = Writing false
        member x.Comment (msg : string) = ()
        member x.Indent() = ()
        member x.Unindent() = ()
        member x.NewLine() = ()
        member x.Offset 
            with get () = 
                System.Diagnostics.Debug.Assert((offset = bw.BaseStream.Position))
                offset
        member x.Seek newOffset = bw.Seek(int newOffset, System.IO.SeekOrigin.Begin) |> ignore; offset <- newOffset

        member x.Int8    name getter _ = bw.Write (getter()); offset <- offset + 1L
        member x.Int16   name getter _ = bw.Write (getter()); offset <- offset + 2L
        member x.Int32   name getter _ = bw.Write (getter()); offset <- offset + 4L
        member x.Int64   name getter _ = bw.Write (getter()); offset <- offset + 8L
        member x.UInt8   name getter _ = bw.Write (getter()); offset <- offset + 1L
        member x.UInt16  name getter _ = bw.Write (getter()); offset <- offset + 2L
        member x.UInt32  name getter _ = bw.Write (getter()); offset <- offset + 4L
        member x.UInt64  name getter _ = bw.Write (getter()); offset <- offset + 8L
        member x.EnumU8  name getter _ infoFunc = bw.Write (fst <| infoFunc (getter())); offset <- offset + 1L
        member x.EnumU16 name getter _ infoFunc = bw.Write (fst <| infoFunc (getter())); offset <- offset + 2L
        member x.EnumU32 name getter _ infoFunc = bw.Write (fst <| infoFunc (getter())); offset <- offset + 4L

        member x.Guid name getter _ =
            let v = getter()
            bw.Write(v.ToByteArray())
            offset <- offset + 16L

        member x.ByteArray name getter _ n = 
            let v = getter()
            bw.Write v
            offset <- offset + (int64 v.Length)
        member x.ByteArray2 name getter _ n _ = 
            let v = getter()
            bw.Write v
            offset <- offset + (int64 v.Length)
        member x.ByteArrayHex name getter _ n = 
            let v = getter()
            bw.Write v
            offset <- offset + (int64 v.Length)

        member x.NullTerminatedString name getter _ =
            let v = getter()
            let bytes = System.Text.Encoding.Unicode.GetBytes(v)
            bw.Write bytes
            bw.Write 0us
            offset <- offset + int64 (bytes.Length + 2) // add 2 bytes for the null terminator

        member x.FixedLengthString name getter _ length =
            let v = getter()
            let bytes = System.Text.Encoding.Unicode.GetBytes(v)
            if (bytes.Length > length + 2) then failwith "Tried to write overlength string"
            bw.Write bytes
            bw.Write(Array.create (length - bytes.Length) 0uy)
            offset <- offset + int64 length // add 2 bytes for the null terminator

        member x.RepeatU8 name v length = bw.Write(Array.create length v); offset <- offset + (int64 length)
        member x.Meta name serializer deserializer = serializer x

type GenericBinaryReader(br : System.IO.BinaryReader) =
    let mutable offset = 0L
    do ()

    interface ISerializer with
        member x.Mode with get () = Reading
        member x.Comment (msg : string) = ()
        member x.Indent() = ()
        member x.Unindent() = ()
        member x.NewLine() = ()
        member x.Offset 
            with get () = 
                System.Diagnostics.Debug.Assert((offset = br.BaseStream.Position))
                offset
        member x.Seek newOffset = br.BaseStream.Seek(newOffset, System.IO.SeekOrigin.Begin) |> ignore; offset <- newOffset

        member x.Int8    name _ setter = setter <| br.ReadSByte();  offset <- offset + 1L
        member x.Int16   name _ setter = setter <| br.ReadInt16();  offset <- offset + 2L
        member x.Int32   name _ setter = setter <| br.ReadInt32();  offset <- offset + 4L
        member x.Int64   name _ setter = setter <| br.ReadInt64();  offset <- offset + 8L
        member x.UInt8   name _ setter = setter <| br.ReadByte();   offset <- offset + 1L
        member x.UInt16  name _ setter = setter <| br.ReadUInt16(); offset <- offset + 2L
        member x.UInt32  name _ setter = setter <| br.ReadUInt32(); offset <- offset + 4L
        member x.UInt64  name _ setter = setter <| br.ReadUInt64(); offset <- offset + 8L
        member x.EnumU8  name _ setter _ = setter <| br.ReadByte();   offset <- offset + 1L
        member x.EnumU16 name _ setter _ = setter <| br.ReadUInt16(); offset <- offset + 2L
        member x.EnumU32 name _ setter _ = setter <| br.ReadUInt32(); offset <- offset + 4L

        member x.Guid name _ setter =
            setter (System.Guid(br.ReadBytes(16)))
            offset <- offset + 16L

        member x.ByteArray name _ setter n = 
            let v = br.ReadBytes(n)
            setter v
            offset <- offset + (int64 v.Length)

        member x.ByteArray2 name _ setter n _ = 
            let v = br.ReadBytes(n)
            setter v
            offset <- offset + (int64 v.Length)

        member x.ByteArrayHex name _ setter n =
            let v = br.ReadBytes(n)
            setter v
            offset <- offset + (int64 v.Length)

        member x.NullTerminatedString name _ setter =
            let rec aux () = seq {
                let bytes = br.ReadBytes(2)
                offset <- offset + 2L
                let codePoint = System.Text.Encoding.Unicode.GetChars(bytes).[0]
                if (codePoint = char 0) then ()
                else
                    yield codePoint
                    yield! aux ()
            }
            let str = aux() |> Array.ofSeq |> System.String
            setter str

        member x.FixedLengthString name _ setter length =
            let rec aux remaining = seq {
                let bytes = br.ReadBytes(2)
                let codePoint = System.Text.Encoding.Unicode.GetChars(bytes).[0]
                if(codePoint = char 0) then 
                    if (remaining > 2) then br.ReadBytes(remaining - 2) |> ignore
                else
                yield codePoint
                if (remaining < 0) then failwith "Non-even string length passed to readUnicodeString"
                if (remaining = 0) then ()
                else yield! aux (remaining - 2)
            }
            let str = aux length |> Array.ofSeq |> System.String
            setter str
            offset <- offset + (int64 length)
            System.Diagnostics.Debug.Assert((offset = br.BaseStream.Position))

        member x.RepeatU8 name v length = 
            let bytes = br.ReadBytes(length)
            for b in bytes do
                if b <> v then failwith "Unexpected value found in repeating byte pattern"
            offset <- offset + (int64 length)

        member x.Meta name serializer deserializer = deserializer x
