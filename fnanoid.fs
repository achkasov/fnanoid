module FNanoid

open System
open System.Security.Cryptography

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
    let mask = (2 <<< int (Math.Floor(Math.Log (float (aLen - 1)) / Math.Log 2.0))) - 1
    let step = ((float aLen) / (1.6 * (float mask) * (float param.size))) |> (Math.Ceiling >> int)

    let inline generateIndexBytes() =
      param.random step
      |> Array.map (fun b -> b &&& byte mask)
      |> Array.filter (fun b -> int b < param.alphabet.Length)

    let rec generateIndexTable table =
      match param.size, table with
      | _, null -> generateIndexTable (generateIndexBytes())
      | trg, tbl when Array.length tbl < trg -> generateIndexTable (Array.concat [| table; generateIndexBytes() |])
      | trg, tbl -> Array.take trg table

    generateIndexTable [||]
    |> Array.map (fun b -> param.alphabet.[int b])
    |> String.Concat


let inline nanoid() = generate defaults

