// include Fake lib
#r @"../../FAKE/tools/FakeLib.dll"
open Fake

//Properties
let buildDir = "./compiled/build"
let testDir  = "./compiled/test/"

//Targets
Target "Clean" (fun _ -> CleanDirs [buildDir;testDir])

Target "BuildApp" (fun _ ->
                   !! "./source/*.fsproj"
                     |> MSBuildDebug buildDir "Build"
                     |> Log "AppBuild-Output: ")

Target "Default" (fun _ ->trace "Hello from FAKE")


//Dependencies
"Clean"
 ==> "BuildApp"
 ==> "Default"

 

//Start bulid
RunTargetOrDefault "BuildApp"
