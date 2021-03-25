#r "paket:
nuget Fake.DotNet.Cli
nuget Fake.IO.FileSystem
nuget Fake.DotNet.Testing.Expecto
nuget Fake.Core.Target //"
#load ".fake/build.fsx/intellisense.fsx"

open Fake.Core
open Fake.DotNet
open Fake.DotNet.Testing
open Fake.IO
open Fake.IO.Globbing.Operators
open Fake.Core.TargetOperators

Target.initEnvironment ()

let testProjects = [ "YamlErd.Tests" ]

type PublishInfo =
    { ProjectName: string
      OutputBinary: string
      Runtimes: string list }

let publishProjects =
    [ { ProjectName = "YamlErd"
        OutputBinary = "yaml-erd"
        Runtimes = [ "win-x64"; "osx-x64"; "linux-x64" ] } ]

Target.create
    "Clean"
    (fun _ ->
        !! "src/**/bin"
        ++ "src/**/obj"
        ++ "tests/**/bin"
        ++ "tests/**/obj"
        ++ "publish/**"
        |> Seq.iter (fun dir -> Shell.rm_rf dir))

Target.create "Build" (fun _ -> !! "src/**/*.*proj" |> Seq.iter (DotNet.build id))

Target.create
    "Test"
    (fun _ ->
        Trace.log "-=-=-=-=-=-=-= Build Test Projects =-=-=-=-=-=-=-"

        !! "tests/**/*.*proj"
        |> Seq.iter (DotNet.build id)

        Trace.log "-=-=-=-=-=-=-= Run Tests =-=-=-=-=-=-=-"

        testProjects
        |> Seq.iter
            (fun proj ->
                !!(sprintf "tests/%s/bin/Release/**/%s.dll" proj proj)
                |> Expecto.run id))

Target.create
    "Publish"
    (fun _ ->
        publishProjects
        |> Seq.iter
            (fun info ->
                let projectPath = sprintf "src/%s" info.ProjectName

                info.Runtimes
                |> Seq.iter
                    (fun runtime ->
                        let outputPath = sprintf "publish/%s" runtime

                        DotNet.publish
                            (fun p ->
                                { p with
                                      Runtime = Some runtime
                                      Configuration = DotNet.BuildConfiguration.Release
                                      SelfContained = Some true
                                      MSBuildParams =
                                          { p.MSBuildParams with
                                                Properties =
                                                    ("PublishSingleFile", "true")
                                                    :: ("PublishTrimmed", "true")
                                                       :: p.MSBuildParams.Properties }
                                      OutputPath = Some outputPath })
                            projectPath

                        let srcPath =
                            sprintf "%s/%s" outputPath
                            <| if runtime.Contains "win" then info.ProjectName + ".exe" else info.ProjectName

                        let distPath =
                            sprintf "%s/%s" outputPath
                            <| if runtime.Contains "win" then info.OutputBinary + ".exe" else info.OutputBinary

                        Shell.cp srcPath distPath)))

Target.create "All" ignore

"Clean" ?=> "Build"
==> "Test"
==> "Publish"
==> "All"

Target.runOrDefault "All"
