import LootCore
open LootCore

def table : LootTable := [(101, 50), (202, 30), (303, 20)]


def smc (s : UInt64) : UInt64 :=
  let z := s + 0x9E3779B97F4A7C15
  let a := (z ^^^ (z >>> 30)) * 0xBF58476D1CE4E5B9
  let b := (a ^^^ (a >>> 27)) * 0x94D049BB133111EB
  b ^^^ (b >>> 31)

def contentionRow (i : Nat) : String :=
  -- four receipts; every 8th round forces a two-way tie to pin the tiebreak
  let t : Nat → Nat := fun j => (smc (UInt64.ofNat (i * 4 + j))).toNat % 1000
  let ts := if i % 8 == 0 then [t 0, t 0, t 2, t 3] else [t 0, t 1, t 2, t 3]
  let reqs : List Request := (List.range 4).map (fun j => (j + 1, ts[j]!))
  match resolve reqs with
  | some res => s!"{i},{ts[0]!},{ts[1]!},{ts[2]!},{ts[3]!},{res.winner}"
  | none => s!"{i},0,0,0,0,0"

def main : IO Unit := do
  IO.FS.createDirAll "build"
  IO.FS.writeFile "build/kernel.slang" LootCore.Slang.lootRollSlang
  let mut out := "seed,index\n"
  for seed in [0:1024] do
    out := out ++ s!"{seed},{rollIndex (UInt32.ofNat seed) table}\n"
  IO.FS.writeFile "build/golden.csv" out
  let cont := "round,t1,t2,t3,t4,winner\n" ++ String.intercalate "\n" ((List.range 64).map contentionRow) ++ "\n"
  IO.FS.writeFile "build/contention_golden.csv" cont
  IO.println s!"wrote build/kernel.slang + build/golden.csv (1024 rows) + build/contention_golden.csv (64 rounds); cumw={cumulativeOf table}"
