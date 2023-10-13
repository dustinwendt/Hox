module ImageHelper

open System
open System.IO
open System.Net.Http
open Avalonia
open Avalonia.Media.Imaging
open Avalonia.Platform


let loadFromFilePath(fp: String) : Bitmap =
  let img = Environment.CurrentDirectory + "/images/" + fp
  new Bitmap(img)
  // System.Drawing.Bitmap(img)
    // new Bitmap()

let loadAvaloniaImage(fp: String) =
    new Avalonia.Media.Imaging.Bitmap(Environment.CurrentDirectory + "/images/" + fp)
    // Image.create[
    //   Image.source b
    // ]

let loadFromResource (resourceUri: Uri) : Bitmap =
    new Bitmap(AssetLoader.Open(resourceUri))

// let loadFromWeb (url: Uri) : (Bitmap option) Task =
//     async {
//       let httpClient = new HttpClient()
//       try{
//         let response = Async.AwaitTask httpClient.GetAsync(url)
//         response.EnsureSuccessStatusCode()
//         let data = Async.AwaitTask response.Content.ReadAsByteArrayAsync()
//         new Bitmap(new MemoryStream(data))
//       }
//       catch{
//         printfn "An error occured while downloading image"
//       }
//     }
