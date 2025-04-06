namespace OpenEtw.Tests
open Xunit
open OpenEtw

//open FsUnit
//open FParsec
//open NUnit.Framework

//open EtwProviderGenerator.Core.SourceHeaderParser
//
//let run p str =
//    match runParserOnString p () "" str with
//    | Success (results, _, _) -> results
//    | Failure (err, _, _) -> failwith err
//
//[<TestFixture>]
//type ParserTests() =
//    [<Fact>] member x.BaseProviderTest()
//        = (run pSourceFile """
//#pragma once
//#include <windows.h>
//// Give the dll name
//// EtwPragma(dll, CsEtw)
//// EtwPragma(provider, CsNamedPipe, CsNamedPipe, 7B587546-851A-4E4C-844D-58F4A4DAD066)
//namespace CsEtw
//{
//	extern "C" // Disable C++ name mangling
//	{
//	}
//}""")
//         |> should equal
//         ({
//
//         })
//

type BasicTests() =
    [<Fact>]
    member this.TestSingleEvent() =
        let events =
            [
                { EtwEvent.empty with id = Some 0us; name = "Test"; cppName = "Test"; symbol = "Test" },
                [Map.empty] // Test cases
            ]

        let name = "Provider"
        let provider =
            {EtwProvider.empty with
                className = name; name = name; symbol = name
                guid      = Util.providerToGuid "Provider"
                events    = events |> List.map fst
            }

        TraceRunner.runTest (nameof this.TestSingleEvent) provider events

    [<Fact>]
    member this.TestBoolEvent() =
        let events =
            [{ EtwEvent.empty with
                id = Some 0us
                name = "Test"
                cppName = "Test"
                symbol = "Test"
                parameters =
                     [
                        {name = "bTest"
                         cppType = "bool"
                         inType = EtwType.Bool
                         outType = EtwOutType.Bool
                         count = EtwCount.Single
                        }
                     ] },
                [ // Test cases
                    [ ("bTest", true  :> obj) ] |> Map.ofList
                    [ ("bTest", false :> obj) ] |> Map.ofList
                ]
            ]

        let name = "Provider"
        let provider =
            {EtwProvider.empty with
                className = name; name = name; symbol = name
                guid      = Util.providerToGuid "Provider"
                events    = events |> List.map fst
            }

        TraceRunner.runTest (nameof this.TestBoolEvent) provider events

    [<Fact>]
    member this.TestComplex() =
        let named n i = {EtwEvent.empty with id = Some i; name = n; cppName = n; symbol = n}
        let events =
            [
                {(named "E0" 0us) with
                    task = Some "T1"
                    level = EtwLevel.warning.name
                    parameters =
                     [
                        {name = "p1"
                         cppType = "char *"
                         inType = EtwType.AnsiString EtwLength.NullTerminated
                         outType = EtwOutType.String
                         count = EtwCount.Single}
                     ] },
                [ // Test cases
                    [ "p1", "Foo" :> obj ] |> Map.ofList
                    [ "p1", ""    :> obj ] |> Map.ofList
                ]
            ]

        let name = "Provider"
        let provider =
            {EtwProvider.empty with
                className = name; name = name; symbol = name
                guid      = Util.providerToGuid "Provider"
                events    = events |> List.map fst
                tasks =
                    [
                        {EtwTask.empty with name = "T1"; id = 1us}
                    ]
            }
        TraceRunner.runTest "TestComplex" provider events

    // Task, level, channel, opcode, keyword not found
    // Too many bla
    // Bla id out of range
    // Names overlength
    // Check for overlength parameter values?
    // Handle splitting overlength parameters to multiple messages, fragment number & total fragment parameters etc.
