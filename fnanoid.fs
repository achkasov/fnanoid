module FNanoid

open System

type Random with
  member this.nextBytes n =
    let mutable buffer = Array.zeroCreate<byte> n
    this.NextBytes buffer
    buffer

type NanoIdParams = { random: int -> byte[]; alphabet: String; size: int }
let defaults =
  { random = (new Random()).nextBytes
    alphabet = "_~0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
    size = 21 }

let generate param =
  match String.length param.alphabet, param.size with
  | a, _ when (a < 1) || (a > 255) -> failwith "Alphabet length must be within 1 and 255 characters."
  | _, size when size < 1 -> failwith "Size must be greater than zero."
  | aLen,  _->
    let mask = ((2 <<< int (Math.Floor(Math.Log (float (aLen - 1)) / Math.Log 2.0))) - 1) |> byte
    let step = ((float aLen) / (1.6 * (float mask) * (float param.size))) |> (Math.Ceiling >> int)

    let inline generateIndexBytes() =
      param.random (26 * step)
      |> Array.map (fun b -> b &&& mask)
      |> Array.filter (fun b -> int b < param.alphabet.Length)

    let rec generateIndexTable table =
      if Array.length table < param.size 
      then generateIndexTable ( Array.concat [| table ; generateIndexBytes() |] )
      else table

    generateIndexTable [||]
    |> Array.take param.size
    |> Array.map (fun b -> param.alphabet.[int b] |> byte)
    |> System.Text.Encoding.Default.GetString

let inline nanoid() = generate defaults

