/*

  A Rubik's Cube Solver

  Copyright (C) 2018 Hideyuki Kawabata

  This solver is based on the CFOP method without F2L.


  Usage: (look at the bottom of this file)

   (a)
    $ ./CubingInSwift
    Input a scramble:
    R  L'  U'  D2  F2  U'  F'  R  D'  U'  B2
    ...

   or

   (b)
    $ ./CubingInSwift
    Input a scrambled pattern:
    OBOWBWYW, BRWYBOWW, ROROWRYG, BBYBGORY, GROBGGOY, RRYGGYWG
    ...


  Note:
  This program is a very simple (almost mechanical) translation from
  a solver written in Haskell.

*/

enum Color : String {
    case White = "W"
    case Red = "R"
    case Blue = "B"
    case Orange = "O"
    case Green = "G"
    case Yellow = "Y"
}

enum Op : String {
    case R; case R_ = "R'"; case U; case U_ = "U'"; case B; case B_ = "B'"; case L; case L_ = "L'"
    case F; case F_ = "F'"; case D; case D_ = "D'"; case Y; case Y_ = "Y'"; case Z; case Z_ = "Z'"
    case N; case M; case M_ = "M'"; case Y2; case Y_2 = "Y'2"; case Z2; case Z_2 = "Z'2"
    case R2; case R_2 = "R'2"; case U2; case U_2 = "U'2"; case B2; case B_2 = "B'2"
    case L2; case L_2 = "L'2"; case F2; case F_2 = "F'2"; case D2; case D_2 = "D'2"
//
    case FstLayer; case SndLayer; case PLL1p 
    case PLL21p; case PLL22p; case PLL23p; case PLL24p; case PLL25p; case PLL26p
    case A2p; case A1p; case Tp; case U1p; case U2p; case Yp; case R2p
    case Zp; case R1p; case Hp; case G2p; case J2p; case G4p; case J1p 
    case G1p; case G3p; case Fp; case Vp; case N2p; case N1p; case Ep
}


// helper functions
func s2c(_ c: String) -> Color {
    switch c {
    case "W": return Color.White
    case "R": return Color.Red
    case "B": return Color.Blue
    case "O": return Color.Orange
    case "G": return Color.Green
    case "Y": return Color.Yellow
    default: return Color.White // error s2c
    }
}


func s2op(_ s: String) -> Op {
    switch s {
    case "R": return Op.R
    case "R'": return Op.R_
    case "U": return Op.U
    case "U'": return Op.U_
    case "B": return Op.B
    case "B'": return Op.B_
    case "L": return Op.L
    case "L'": return Op.L_
    case "F": return Op.F
    case "F'": return Op.F_
    case "D": return Op.D
    case "D'": return Op.D_
    case "Y": return Op.Y
    case "Y'": return Op.Y_
    case "Z": return Op.Z
    case "Z'": return Op.Z_
    case "N": return Op.N
    case "M": return Op.M
    case "M'": return Op.M_
    case "Y2": return Op.Y2
    case "Y'2": return Op.Y_2
    case "Z2": return Op.Z2
    case "Z'2": return Op.Z_2
    case "R2": return Op.R2
    case "R'2": return Op.R_2
    case "U2": return Op.U2
    case "U'2": return Op.U_2
    case "B2": return Op.B2
    case "B'2": return Op.B_2
    case "L2": return Op.L2
    case "L'2": return Op.L_2
    case "F2": return Op.F2
    case "F'2": return Op.F_2
    case "D2": return Op.D2
    case "D'2": return Op.D_2
           
//
    case "FstLayer": return Op.FstLayer
    case "SndLayer": return Op.SndLayer
    case "PLL1p": return Op.PLL1p
    case "PLL21p": return Op.PLL21p
    case "PLL22p": return Op.PLL22p
    case "PLL23p": return Op.PLL23p
    case "PLL24p": return Op.PLL24p
    case "PLL25p": return Op.PLL25p
    case "PLL26p": return Op.PLL26p
    case "A2p": return Op.A2p
    case "A1p": return Op.A1p
    case "Tp": return Op.Tp
    case "U1p": return Op.U1p
    case "U2p": return Op.U2p
    case "Yp": return Op.Yp
    case "R2p": return Op.R2p
    case "Zp": return Op.Zp
    case "R1p": return Op.R1p
    case "Hp": return Op.Hp
    case "G2p": return Op.G2p
    case "J2p": return Op.J2p
    case "G4p": return Op.G4p
    case "J1p": return Op.J1p
    case "G1p": return Op.G1p
    case "G3p": return Op.G3p
    case "Fp": return Op.Fp
    case "Vp": return Op.Vp
    case "N2p": return Op.N2p
    case "N1p": return Op.N1p
    case "Ep": return Op.Ep
    default: return Op.N // error s2op
    }
}


// simple parser
func fromString(_ str: String) -> [Op] {
    func iter(_ xxs_arg: [String], _ acc_arg: [Op]) -> [Op] {
        var xxs = xxs_arg
        var acc = acc_arg
        if xxs.count == 0 { // length: 0
            return acc
        }
        let x = xxs.remove(at: 0) // length: >= 1
        var xs = xxs
        if x == " " || x == "," {
            return iter(xs, acc)
        }
        if x == "R" || x == "U" || x == "B" || x == "L" || x == "F" ||
             x == "D" || x == "Y" || x == "Z" || x == "M" || x == "N" {
            if xs.count >= 1 {
                let xs0 = xs[0]
                if xs0 == "'" {
                    xs.remove(at: 0)
                    if xs.count >= 1 {
                        let xs1 = xs[0]
                        if xs1 == "2" { // e.g. U'2
                            xs.remove(at: 0)
                            acc.append(s2op(x + xs0 + xs1))
                            return iter(xs, acc)
                        } else {
                            acc.append(s2op(x + xs0))
                            return iter(xs, acc)
                        }
                    } else {
                        acc.append(s2op(x + xs0))
                        return iter(xs, acc)
                    }
                } else if xs0 == "2" {
                    xs.remove(at: 0)
                    acc.append(s2op(x + xs0))
                    return iter(xs, acc)
                } else {
                    acc.append(s2op(x))
                    return iter(xs, acc)
                }
            } else {
                acc.append(s2op(x))
                return iter(xs, acc)
            }
        } else {
            acc.append(s2op(x))
//            acc.append("error_fromString")
            return acc
        }
    }
    return iter(str.characters.map({"\($0)"}), [])
}


func fromStringPos(_ str: String) -> Cube {
    let segs = str.characters.split(separator: ",")
    let collist = segs.map({$0.filter({$0 != " "}).map({"\($0)"})})
    let sw = collist[0]
    let sr = collist[1]
    let sb = collist[2]
    let so = collist[3]
    let sg = collist[4]
    let sy = collist[5]
    return Cube(w: Surface(s2c(sw[0]), s2c(sw[1]), s2c(sw[2]), s2c(sw[3]),
                           s2c(sw[4]), s2c(sw[5]), s2c(sw[6]), s2c(sw[7])),
                r: Surface(s2c(sr[0]), s2c(sr[1]), s2c(sr[2]), s2c(sr[3]),
                           s2c(sr[4]), s2c(sr[5]), s2c(sr[6]), s2c(sr[7])),
                b: Surface(s2c(sb[0]), s2c(sb[1]), s2c(sb[2]), s2c(sb[3]),
                           s2c(sb[4]), s2c(sb[5]), s2c(sb[6]), s2c(sb[7])),
                o: Surface(s2c(so[0]), s2c(so[1]), s2c(so[2]), s2c(so[3]),
                           s2c(so[4]), s2c(so[5]), s2c(so[6]), s2c(so[7])),
                g: Surface(s2c(sg[0]), s2c(sg[1]), s2c(sg[2]), s2c(sg[3]),
                           s2c(sg[4]), s2c(sg[5]), s2c(sg[6]), s2c(sg[7])),
                y: Surface(s2c(sy[0]), s2c(sy[1]), s2c(sy[2]), s2c(sy[3]),
                           s2c(sy[4]), s2c(sy[5]), s2c(sy[6]), s2c(sy[7])))
}


func revOp(_ x: Op) -> Op {
    switch x {
    case Op.U: return Op.U_
    case Op.L: return Op.L_
    case Op.R: return Op.R_
    case Op.B: return Op.B_
    case Op.D: return Op.D_
    case Op.F: return Op.F_
    case Op.Y: return Op.Y_
    case Op.Z: return Op.Z_
    case Op.U_: return Op.U
    case Op.L_: return Op.L
    case Op.R_: return Op.R
    case Op.B_: return Op.B
    case Op.D_: return Op.D
    case Op.F_: return Op.F
    case Op.Y_: return Op.Y
    case Op.Z_: return Op.Z
    default: return x
    }
}

func expandOp(_ xxs_arg: [Op]) -> [Op] {
    var xxs = xxs_arg
    if xxs.count == 0 { // length: 0
        return []
    }
    let x = xxs.remove(at: 0) // length: >=1
    let xs = xxs
    switch x {
    case Op.R2: return [Op.R, Op.R] + expandOp(xs)
    case Op.L2: return [Op.L, Op.L] + expandOp(xs)
    case Op.D2: return [Op.D, Op.D] + expandOp(xs)
    case Op.U2: return [Op.U, Op.U] + expandOp(xs)
    case Op.B2: return [Op.B, Op.B] + expandOp(xs)
    case Op.F2: return [Op.F, Op.F] + expandOp(xs)
    case Op.Y2: return [Op.Y, Op.Y] + expandOp(xs)
    case Op.Z2: return [Op.Z, Op.Z] + expandOp(xs)
    case Op.R_2: return [Op.R, Op.R] + expandOp(xs)
    case Op.L_2: return [Op.L, Op.L] + expandOp(xs)
    case Op.D_2: return [Op.D, Op.D] + expandOp(xs)
    case Op.U_2: return [Op.U, Op.U] + expandOp(xs)
    case Op.B_2: return [Op.B, Op.B] + expandOp(xs)
    case Op.F_2: return [Op.F, Op.F] + expandOp(xs)
    case Op.Y_2: return [Op.Y, Op.Y] + expandOp(xs)
    case Op.Z_2: return [Op.Z, Op.Z] + expandOp(xs)
    default: return [x] + expandOp(xs)
    }
}


func exchangeOp(_ xxs_arg: [Op]) -> [Op] {
    var xxs = xxs_arg
    if xxs.count == 0 { // length: 0
        return []
    }
    let x = xxs.remove(at: 0)
    if xxs.count == 0 { // length: 1
        return [x]
    }
    let y = xxs.remove(at: 0) // length: >=2
    let xs = xxs
    switch (x, y) {
        // Y
    case (Op.Y, Op.U): return [y] + exchangeOp([x] + xs)
    case (Op.Y, Op.U_): return [y] + exchangeOp([x] + xs)
    case (Op.Y_, Op.U): return [y] + exchangeOp([x] + xs)
    case (Op.Y_, Op.U_): return [y] + exchangeOp([x] + xs)
    case (Op.Y, Op.D): return [y] + exchangeOp([x] + xs)
    case (Op.Y, Op.D_): return [y] + exchangeOp([x] + xs)
    case (Op.Y_, Op.D): return [y] + exchangeOp([x] + xs)
    case (Op.Y_, Op.D_): return [y] + exchangeOp([x] + xs)
                       // D
    case (Op.D, Op.U): return [y] + exchangeOp([x] + xs)
    case (Op.D, Op.U_): return [y] + exchangeOp([x] + xs)
    case (Op.D_, Op.U): return [y] + exchangeOp([x] + xs)
    case (Op.D_, Op.U_): return [y] + exchangeOp([x] + xs)
                       // B
    case (Op.B, Op.F): return [y] + exchangeOp([x] + xs)
    case (Op.B, Op.F_): return [y] + exchangeOp([x] + xs)
    case (Op.B_, Op.F): return [y] + exchangeOp([x] + xs)
    case (Op.B_, Op.F_): return [y] + exchangeOp([x] + xs)
                       // L
    case (Op.L, Op.R): return [y] + exchangeOp([x] + xs)
    case (Op.L, Op.R_): return [y] + exchangeOp([x] + xs)
    case (Op.L_, Op.R): return [y] + exchangeOp([x] + xs)
    case (Op.L_, Op.R_): return [y] + exchangeOp([x] + xs)
                       //
    default: return [x] + exchangeOp([y] + xs)
    }
}


func reduceOp(_ xxs_arg: [Op]) -> [Op] {
    var xxs = xxs_arg
    if xxs.count == 0 { // length: 0
        return []
    }
    let x = xxs.remove(at: 0)
    let xs = xxs
    if xxs.count == 0 { // length: 1
        return [x]
    }
    let y = xxs.remove(at: 0) // length: >=2
    let ys = xxs
    if x == revOp(y) {
        return reduceOp(ys)
    }
    if xxs.count >= 2 { // length: >=4
        let z = xxs.remove(at: 0)
        let zs = xxs
        let w = xxs.remove(at: 0)
        let ws = xxs
        if x == y && y == z && z == w {
            return reduceOp(ws)
        } else if x == y && y == z {
            return reduceOp([revOp(x)] + zs)
        } else {
            return [x] + reduceOp(xs)
        }
    } else if xxs.count == 1 { // length: 3
        let z = xxs.remove(at: 0)
        if x == y && y == z {
            return [revOp(x)]
        } else {
            return [x] + reduceOp(xs)
        }
    } else { // length: 2, x != y
        return xxs_arg
    }
}


func iterOpt(_ f: ([Op]) -> [Op], _ l: [Op]) -> [Op] {
    let l2 = f(l)
    if l == l2 {
        return l2
    } else {
        return iterOpt(f, l2)
    }
}

func mergeOp(_ xxs_arg: [Op]) -> [Op] {
    var xxs = xxs_arg
    if xxs.count == 0 { // length: 0
        return []
    }
    let x = xxs.remove(at: 0)
    let xs = xxs
    if xxs.count == 0 { // length: 1
        return [x]
    }
    let y = xxs.remove(at: 0)
    let ys = xxs
    switch (x, y) {
    case (Op.U, Op.U): return [Op.U2] + mergeOp(ys)
    case (Op.B, Op.B): return [Op.B2] + mergeOp(ys)
    case (Op.F, Op.F): return [Op.F2] + mergeOp(ys)
    case (Op.D, Op.D): return [Op.D2] + mergeOp(ys)
    case (Op.L, Op.L): return [Op.L2] + mergeOp(ys)
    case (Op.R, Op.R): return [Op.R2] + mergeOp(ys)
    case (Op.Y, Op.Y): return [Op.Y2] + mergeOp(ys)
    case (Op.Z, Op.Z): return [Op.Z2] + mergeOp(ys)
    case (Op.U_, Op.U_): return [Op.U2] + mergeOp(ys)
    case (Op.B_, Op.B_): return [Op.B2] + mergeOp(ys)
    case (Op.F_, Op.F_): return [Op.F2] + mergeOp(ys)
    case (Op.D_, Op.D_): return [Op.D2] + mergeOp(ys)
    case (Op.L_, Op.L_): return [Op.L2] + mergeOp(ys)
    case (Op.R_, Op.R_): return [Op.R2] + mergeOp(ys)
    case (Op.Y_, Op.Y_): return [Op.Y2] + mergeOp(ys)
    case (Op.Z_, Op.Z_): return [Op.Z2] + mergeOp(ys)
    default: return [x] + mergeOp(xs)
    }
}

func optimizeOp(_ l: [Op]) -> [Op] {
    var l = expandOp(l)
    l = iterOpt(exchangeOp, l)
    l = iterOpt(reduceOp, l)
    l = mergeOp(l)
    return l
}


// solver
func solveQ(_ q: Cube) -> [Op] {
    var pair = (q, [Op.FstLayer])
    pair = step(pair, setRY(Op.N))
    pair = step(pair, {[Op.Y] + setRY(Op.Y)($0.dupCube().turn(Op.Y)) + [Op.Y_]})
    pair = step(pair, {[Op.Y2] + setRY(Op.Y2)($0.dupCube().turn(Op.Y2)) + [Op.Y2]})
    pair = step(pair, {[Op.Y_] + setRY(Op.Y_)($0.dupCube().turn(Op.Y_)) + [Op.Y]})
    pair = step(pair, {(_) in [Op.SndLayer]})
    pair = step(pair, setYGR(Op.N))
    pair = step(pair, {[Op.Y] + setYGR(Op.Y)($0.dupCube().turn(Op.Y)) + [Op.Y_]})
    pair = step(pair, {[Op.Y2] + setYGR(Op.Y2)($0.dupCube().turn(Op.Y2)) + [Op.Y2]})
    pair = step(pair, {[Op.Y_] + setYGR(Op.Y_)($0.dupCube().turn(Op.Y_)) + [Op.Y]})
    pair = step(pair, setGR(Op.N))
    pair = step(pair, {[Op.Y] + setGR(Op.Y)($0.dupCube().turn(Op.Y)) + [Op.Y_]})
    pair = step(pair, {[Op.Y2] + setGR(Op.Y2)($0.dupCube().turn(Op.Y2)) + [Op.Y2]})
    pair = step(pair, {[Op.Y_] + setGR(Op.Y_)($0.dupCube().turn(Op.Y_)) + [Op.Y]})
    pair = step(pair, oneToThree)
    pair = step(pair, threeToFive)
    pair = step(pair, fiveToNine)
    pair = step(pair, {[Op.Y] + fiveToNine($0.dupCube().turn(Op.Y)) + [Op.Y_]})
    pair = step(pair, {[Op.Y2] + fiveToNine($0.dupCube().turn(Op.Y2)) + [Op.Y2]})
    pair = step(pair, {[Op.Y_] + fiveToNine($0.dupCube().turn(Op.Y_)) + [Op.Y]})
    pair = step(pair, nineToFinish)
    pair = step(pair, {[Op.Y] + nineToFinish($0.dupCube().turn(Op.Y)) + [Op.Y_]})
    pair = step(pair, {[Op.Y2] + nineToFinish($0.dupCube().turn(Op.Y2)) + [Op.Y2]})
    pair = step(pair, {[Op.Y_] + nineToFinish($0.dupCube().turn(Op.Y_)) + [Op.Y]})
    pair = step(pair, finishQ)
    let (_, ops) = pair
    return optimizeOp(ops)
}

func step(_ pair: (Cube, [Op]), _ slvr: (Cube) -> [Op]) -> (Cube, [Op]) {
    let (q, ops) = pair
    let ops2 = slvr(q)
    let q2 = q.dupCube().applySeq(ops2)
    return (q2, ops + ops2)
}

func finishQ(_ q: Cube) -> [Op] {
    if q.check() { return []
    } else if q.dupCube().turn(Op.U).check() { return [Op.U]
    } else if q.dupCube().turn(Op.U2).check() { return [Op.U2]
    } else if q.dupCube().turn(Op.U_).check() { return [Op.U_]
    } else { return [Op.U] // error_finishQ
    }
}


func rotc(_ op:Op, _ c:Color) -> Color {
    switch op {
    case Op.N: return c
    case Op.Y:
        switch c {
        case Color.Red: return Color.Blue
        case Color.Blue: return Color.Orange
        case Color.Orange: return Color.Green
        case Color.Green: return Color.Red
        default: return c
        }
    case Op.Y2:
        return rotc(Op.Y, (rotc(Op.Y, c)))
    case Op.Y_:
        return rotc(Op.Y, (rotc(Op.Y2, c)))
    default:
        return c
    }
            
}


func setRY(_ tc: Op) -> (_ q: Cube) -> [Op] {
  func setRY2(_ q: Cube) -> [Op] {
    let sr = q.r.getColor
    let sy = q.y.getColor
    let sb = q.b.getColor
    let sw = q.w.getColor
    let sg = q.g.getColor
    let so = q.o.getColor
    let yellow = rotc(tc, Color.Yellow)
    let red = rotc(tc, Color.Red)

    if sr(2) == red && sy(6) == yellow { return []
//
    } else if sr(2) == yellow && sy(6) == red { return [Op.F_, Op.D, Op.R_, Op.D_]
    } else if sr(4) == yellow && sb(8) == red { return [Op.D, Op.R_, Op.D_]
    } else if sr(4) == red && sb(8) == yellow { return [Op.F]
    } else if sr(6) == red && sw(2) == yellow { return [Op.F, Op.F]
    } else if sr(6) == yellow && sw(2) == red { return [Op.U_, Op.R_, Op.F, Op.R]
    } else if sr(8) == yellow && sg(4) == red { return [Op.D_, Op.L, Op.D]
    } else if sr(8) == red && sg(4) == yellow { return [Op.F_]
//
    } else if sb(2) == red && sy(4) == yellow { return [Op.R, Op.D, Op.R_, Op.D_]
    } else if sb(2) == yellow && sy(4) == red { return [Op.R, Op.F]
    } else if sb(4) == yellow && so(8) == red { return [Op.B, Op.U, Op.U, Op.B_, Op.F, Op.F]
    } else if sb(4) == red && so(8) == yellow { return [Op.R_, Op.U, Op.R, Op.F, Op.F]
    } else if sb(6) == red && sw(4) == yellow { return [Op.U, Op.F, Op.F]
    } else if sb(6) == yellow && sw(4) == red { return [Op.R_, Op.F, Op.R]
//
    } else if so(2) == red && sy(2) == yellow { return [Op.B, Op.B, Op.U, Op.U, Op.F, Op.F]
    } else if so(2) == yellow && sy(2) == red { return [Op.B, Op.B, Op.U, Op.R_, Op.F, Op.R]
    } else if so(4) == yellow && sg(8) == red { return [Op.L, Op.U_, Op.L_, Op.F, Op.F]
    } else if so(4) == red && sg(8) == yellow { return [Op.B_, Op.U_, Op.B, Op.U_, Op.F, Op.F]
    } else if so(6) == red && sw(6) == yellow { return [Op.U, Op.U, Op.F, Op.F]
    } else if so(6) == yellow && sw(6) == red { return [Op.U_, Op.L, Op.F_, Op.L_]
//
    } else if sg(2) == red && sy(8) == yellow { return [Op.L_, Op.D_, Op.L, Op.D]
    } else if sg(2) == yellow && sy(8) == red { return [Op.L_, Op.F_]
    } else if sg(6) == red && sw(8) == yellow { return [Op.U_, Op.F, Op.F]
    } else if sg(6) == yellow && sw(8) == red { return [Op.L, Op.F_, Op.L_]
    } else { return [Op.U] // error_setRY
    }
  }
  return setRY2
}


func setYGR(_ tc: Op) -> (_ q: Cube) -> [Op] {
  func setYGR2(_ q: Cube) -> [Op] {
    let sr = q.r.getColor
    let sb = q.b.getColor
    let sg = q.g.getColor
    let so = q.o.getColor
    let yellow = rotc(tc, Color.Yellow)
    let green = rotc(tc, Color.Green)
    let red = rotc(tc, Color.Red)

    if sr(1) == red && sg(3) == green { return []
//
    } else if sr(1) == yellow && sg(3) == red { return [Op.F, Op.U, Op.F_, Op.U_, Op.F, Op.U, Op.F_]
    } else if sr(1) == green && sg(3) == yellow { return [Op.L_, Op.U_, Op.L, Op.U, Op.L_, Op.U_, Op.L]
//
    } else if sr(3) == yellow && sb(1) == green { return [Op.F_, Op.U_, Op.F, Op.U, Op.U, Op.L_, Op.U_, Op.L]
    } else if sr(3) == red && sb(1) == yellow { return [Op.R, Op.U, Op.R_, Op.F, Op.U, Op.F_]
    } else if sr(3) == green && sb(1) == red { return [Op.R, Op.U, Op.R_, Op.L_, Op.U_, Op.L]
//
    } else if sr(5) == green && sb(7) == yellow { return [Op.L_, Op.U, Op.L]
    } else if sr(5) == yellow && sb(7) == red { return [Op.U, Op.L_, Op.U_, Op.L]
    } else if sr(5) == red && sb(7) == green { return [Op.U, Op.L_, Op.U, Op.L, Op.U_, Op.U_, Op.L_, Op.U_, Op.L]
//
    } else if sr(7) == red && sg(5) == yellow { return [Op.L_, Op.U_, Op.L]
    } else if sr(7) == green && sg(5) == red { return [Op.F, Op.U, Op.U, Op.F_, Op.U_, Op.F, Op.U, Op.F_]
    } else if sr(7) == yellow && sg(5) == green { return [Op.F, Op.U, Op.F_]
//
    } else if sb(3) == green && so(1) == red { return [Op.R_, Op.U_, Op.U_, Op.R, Op.F, Op.U, Op.F_]
    } else if sb(3) == yellow && so(1) == green { return [Op.R_, Op.U_, Op.R, Op.U_, Op.L_, Op.U_, Op.L]
    } else if sb(3) == red && so(1) == yellow { return [Op.B, Op.U, Op.B_, Op.U, Op.F, Op.U, Op.F_]
//
    } else if sb(5) == green && so(7) == yellow { return [Op.U, Op.U, Op.F, Op.U, Op.F_]
    } else if sb(5) == red && so(7) == green { return [Op.U, Op.U, Op.L_, Op.U, Op.L, Op.U, Op.U, Op.L_, Op.U_, Op.L]
    } else if sb(5) == yellow && so(7) == red { return [Op.U, Op.U, Op.L_, Op.U_, Op.L]
//
    } else if so(3) == red && sg(1) == yellow { return [Op.L, Op.U, Op.L_, Op.U, Op.U, Op.F, Op.U, Op.F_]
    } else if so(3) == yellow && sg(1) == green { return [Op.B_, Op.U_, Op.B, Op.L_, Op.U_, Op.L]
    } else if so(3) == green && sg(1) == red { return [Op.L, Op.U_, Op.L_, Op.U_, Op.F, Op.U, Op.F_]
//
    } else if so(5) == yellow && sg(7) == red { return [Op.F, Op.U_, Op.F_]
    } else if so(5) == green && sg(7) == yellow { return [Op.U_, Op.F, Op.U, Op.F_]
    } else if so(5) == red && sg(7) == green { return [Op.F, Op.U, Op.U, Op.F_, Op.L_, Op.U_, Op.L]
    } else { return [Op.U] // error_setYGR
    }
  }
  return setYGR2
}


func setGR(_ tc: Op) -> (_ q: Cube) -> [Op] {
  func setGR2(_ q: Cube) -> [Op] {
    let sr = q.r.getColor
    let sg = q.g.getColor
    let sw = q.w.getColor
    let sb = q.b.getColor
    let so = q.o.getColor
    let green = rotc(tc, Color.Green)
    let red = rotc(tc, Color.Red)

    if sr(8) == red && sg(4) == green { return []
//
    } else if sr(8) == green && sg(4) == red {
        return [Op.F, Op.U_, Op.F_, Op.U_, Op.L_, Op.U, Op.L, Op.U_, Op.F, Op.U_, Op.F_, Op.U_, Op.L_, Op.U, Op.L]
    } else if sr(6) == red && sw(2) == green { return [Op.U_, Op.L_, Op.U, Op.L, Op.U, Op.F, Op.U_, Op.F_]
    } else if sr(6) == green && sw(2) == red { return [Op.U, Op.U, Op.F, Op.U_, Op.F_, Op.U_, Op.L_, Op.U, Op.L]
    } else if sr(4) == green && sb(8) == red { 
        return [Op.R, Op.U_, Op.R_, Op.U_, Op.F_, Op.U, Op.F, Op.U, Op.L_, Op.U, Op.L, Op.U, Op.F, Op.U_, Op.F_]
    } else if sr(4) == red && sb(8) == green {
        return [Op.R, Op.U_, Op.R_, Op.U_, Op.F_, Op.U, Op.F, Op.F, Op.U_, Op.F_, Op.U_, Op.L_, Op.U, Op.L]
//
    } else if sb(4) == green && so(8) == red {
        return [Op.Y, Op.R, Op.U_, Op.R_, Op.U_, Op.F_, Op.U, Op.F, Op.Y_, Op.U, Op.U, Op.L_, Op.U, Op.L, Op.U, Op.F, Op.U_, Op.F_]
    } else if sb(4) == red && so(8) == green {
        return [Op.Y, Op.R, Op.U_, Op.R_, Op.U_, Op.F_, Op.U, Op.F, Op.Y_, Op.U, Op.F, Op.U_, Op.F_, Op.U_, Op.L_, Op.U, Op.L]
    } else if sb(6) == red && sw(4) == green { return [Op.L_, Op.U, Op.L, Op.U, Op.F, Op.U_, Op.F_]
    } else if sb(6) == green && sw(4) == red { return [Op.U_, Op.F, Op.U_, Op.F_, Op.U_, Op.L_, Op.U, Op.L]
//
    } else if so(6) == red && sw(6) == green { return [Op.U, Op.L_, Op.U, Op.L, Op.U, Op.F, Op.U_, Op.F_]
    } else if so(6) == green && sw(6) == red { return [Op.F, Op.U_, Op.F_, Op.U_, Op.L_, Op.U, Op.L]
    } else if so(4) == red && sg(8) == green {
        return [Op.Y, Op.Y, Op.R, Op.U_, Op.R_, Op.U_, Op.F_, Op.U, Op.F, Op.Y, Op.Y, Op.U, Op.U, Op.F, Op.U_, Op.F_, Op.U_, Op.L_, Op.U, Op.L]
    } else if so(4) == green && sg(8) == red {
        return [Op.Y, Op.Y, Op.R, Op.U_, Op.R_, Op.U_, Op.F_, Op.U, Op.F, Op.Y, Op.Y, Op.U_, Op.L_, Op.U, Op.L, Op.U, Op.F, Op.U_, Op.F_]
//
    } else if sg(6) == red && sw(8) == green { return [Op.U, Op.U, Op.L_, Op.U, Op.L, Op.U, Op.F, Op.U_, Op.F_]
    } else if sg(6) == green && sw(8) == red { return [Op.U, Op.F, Op.U_, Op.F_, Op.U_, Op.L_, Op.U, Op.L]
    } else { return [Op.U] // error_setGR
    }
  }
  return setGR2
}

// PLL1
func oneToThree(_ q: Cube) -> [Op] {
    let swq = q.w.getColor
    if swq(2) != Color.White && swq(4) != Color.White &&
         swq(6) != Color.White && swq(8) != Color.White { 
        return [Op.PLL1p] + [Op.F, Op.R, Op.U, Op.R_, Op.U_, Op.F_]
    } else { return []
    }
}

// PLL2
func threeToFive(_ q: Cube) -> [Op] {
    let sq = q.w.getColor
    let (w2, w4, w6, w8) = (sq(2), sq(4), sq(6), sq(8))
    
    if w2 == w4 && w2 != w6 && w2 != w8 { return [Op.PLL21p] + [Op.B, Op.U, Op.L, Op.U_, Op.L_, Op.B_]
    } else if  w4 == w6 && w4 != w8 && w4 != w2 { return [Op.PLL22p] + [Op.U, Op.B, Op.U, Op.L, Op.U_, Op.L_, Op.B_]
    } else if  w6 == w8 && w6 != w2 && w6 != w4 { return [Op.PLL23p] + [Op.U, Op.U, Op.B, Op.U, Op.L, Op.U_, Op.L_, Op.B_]
    } else if  w8 == w2 && w8 != w4 && w8 != w6 { return [Op.PLL24p] + [Op.U_, Op.B, Op.U, Op.L, Op.U_, Op.L_, Op.B_]
    } else if  w4 == w8 && w4 != w2 && w4 != w6 { return [Op.PLL25p] + [Op.F, Op.R, Op.U, Op.R_, Op.U_, Op.F_]
    } else if  w2 == w6 && w2 != w4 && w2 != w8 { return [Op.PLL26p] + [Op.U, Op.F, Op.R, Op.U, Op.R_, Op.U_, Op.F_]
    } else { return []
    }
}

// PLL3
func fiveToNine(_ q: Cube) -> [Op] {
    let (r5, r7) = (q.r.getColor(5), q.r.getColor(7))
    let (o5, o7) = (q.o.getColor(5), q.o.getColor(7))
    let (g5, g7) = (q.g.getColor(5), q.g.getColor(7))
    let (b5, b7) = (q.b.getColor(5), q.b.getColor(7))
    let wq = q.w.getColor
    let (w1, w3, w5, w7) = (wq(1), wq(3), wq(5), wq(7))

    if r5 == o7 && o7 == g5 && g5 == g7 {
        return [Op.R, Op.U, Op.U, Op.R_, Op.R_, Op.U_, Op.R, Op.R, Op.U_, Op.R_, Op.R_, Op.U, Op.U, Op.R]
    } else if  w1 == w3 && w3 == o5 && o5 == o7 {
        return [Op.R, Op.R, Op.D_, Op.R, Op.U, Op.U, Op.R_, Op.D, Op.R, Op.U, Op.U, Op.R]
    } else if  r5 == b5 && b5 == o5 && o5 == w1 { return [Op.R, Op.U, Op.R_, Op.U, Op.R, Op.U_, Op.U_, Op.R_]
    } else if  r7 == b7 && b7 == g7 && g7 == w5 { return [Op.R, Op.U_, Op.U_, Op.R_, Op.U_, Op.R, Op.U_, Op.R_]
    } else if  r7 == r5 && r5 == o7 && o7 == o5 {
        return [Op.R, Op.U_, Op.U_, Op.R_, Op.U_, Op.R, Op.U, Op.R_, Op.U_, Op.R, Op.U_, Op.R_]
    } else if  r7 == b5 && b5 == w3 && w3 == w7 {
        return [Op.L, Op.F_, Op.F_, Op.R_, Op.R_, Op.D, Op.R, Op.D_, Op.R, Op.F_, Op.F_, Op.L_]
    } else if  r7 == w3 && w3 == w5 && w5 == o5 { return [Op.L, Op.F, Op.R_, Op.F_, Op.L_, Op.F, Op.R, Op.F_]
    } else { return []
    }
}


// OLL
func nineToFinish(_ q: Cube) -> [Op] {
    let (r5, r6, r7) = (q.r.getColor(5), q.r.getColor(6), q.r.getColor(7))
    let (g5, g6, g7) = (q.g.getColor(5), q.g.getColor(6), q.g.getColor(7))
    let (o5, o6, o7) = (q.o.getColor(5), q.o.getColor(6), q.o.getColor(7))
    let (b5, b6, b7) = (q.b.getColor(5), q.b.getColor(6), q.b.getColor(7))

    if g6 == g7 && o5 == o6 && b5 == b7 && b7 == r6 { // A1
        return [Op.A1p] + [Op.R, Op.R, Op.F, Op.F, Op.R_, Op.B_, Op.R, Op.F, Op.F, Op.R_, Op.B, Op.R_]
    } else if  g6 == g7 && o5 == o6 && r5 == r7 && r7 == b6 { // A2
        return [Op.A2p] + [Op.R, Op.B_, Op.R, Op.F, Op.F, Op.R_, Op.B, Op.R, Op.F, Op.F, Op.R_, Op.R_]
    } else if  r6 == r7 && g5 == g7 && g5 != g6 && o5 == o6 { // T
        return [Op.Tp] + [Op.R, Op.U, Op.R_, Op.U_, Op.R_, Op.F, Op.R, Op.R, Op.U_, Op.R_, Op.U_, Op.R, Op.U, Op.R_, Op.F_]
    } else if  o5 == o6 && o5 == o7 && r5 != r6 && r5 == r7 { 
        if r5 == b6 {
            return [Op.U1p] + [Op.R, Op.R, Op.U, Op.R, Op.U, Op.R_, Op.U_, Op.R_, Op.U_, Op.R_, Op.U, Op.R_] // U1
        } else {
            return [Op.U2p] + [Op.R, Op.U_, Op.R, Op.U, Op.R, Op.U, Op.R, Op.U_, Op.R_, Op.U_, Op.R_, Op.R_] // U2
        }
    } else if  r6 == r7 && b5 == b6 && r7 == o5 && b6 != b7 { // Y
        return [Op.Yp] + [Op.F, Op.R, Op.U_, Op.R_, Op.U_, Op.R, Op.U, Op.R_, Op.F_, Op.R, Op.U, Op.R_, Op.U_, Op.R_, Op.F, Op.R, Op.F_]
    } else if  r5 == r7 && r5 == b6 && r6 == b7 && g5 == g6 { // R2
        return [Op.R2p] + [Op.R_, Op.U_, Op.U_, Op.R, Op.U_, Op.U_, Op.R_, Op.F, Op.R, Op.U, Op.R_, Op.U_, Op.R_, Op.F_, Op.R, Op.R, Op.U_]
    } else if  r5 == r7 && r5 == b6 && r6 == b7 && b5 == b7 { // Z
        return [Op.Zp] + [Op.R_, Op.L, Op.F_, Op.R, Op.R, Op.L_, Op.L_, Op.B_, Op.R, Op.R, Op.L_, Op.L_, Op.F_, Op.R_, Op.L, Op.D, Op.D, Op.R, Op.R, Op.L_, Op.L_, Op.U]
    } else if  r5 == r7 && r5 == g6 && r6 == g5 && g7 != g5 { // R1
        return [Op.R1p] + [Op.L, Op.U, Op.U, Op.L_, Op.U, Op.U, Op.L, Op.F_, Op.L_, Op.U_, Op.L, Op.U, Op.L, Op.F, Op.L_, Op.L_, Op.U]
    } else if  r5 == r7 && b5 == b7 && r5 == o6 && b5 == g6 { // H
        return [Op.Hp] + [Op.M, Op.M, Op.U_, Op.M, Op.M, Op.U_, Op.U_, Op.M, Op.M, Op.U_, Op.M, Op.M]
    } else if  r5 == r6 && g5 == g7 && g7 == b6 && b7 == g6 { // G1
        return [Op.G1p] + [Op.R, Op.R, Op.D, Op.Y, Op.R_, Op.U, Op.R_, Op.U_, Op.R, Op.D_, Op.Y_, Op.R_, Op.R_, Op.F_, Op.U, Op.F]
    } else if  r5 == r6 && r6 == o7 && g5 == g6 && g6 == g7 { // J2
        return [Op.J2p] + [Op.R, Op.U, Op.R_, Op.F_, Op.R, Op.U, Op.R_, Op.U_, Op.R_, Op.F, Op.R, Op.R, Op.U_, Op.R_, Op.U_]
    } else if  r7 == r6 && r6 == o5 && b5 == b7 && b7 == g6 { // G3
        return [Op.G3p] + [Op.L_, Op.L_, Op.D_, Op.Y_, Op.L, Op.U_, Op.L, Op.U, Op.L_, Op.D, Op.Y, Op.L, Op.L, Op.F, Op.U_, Op.F_]
    } else if  r7 == r6 && r6 == o5 && b5 == b6 && b6 == b7 { // J1
        return [Op.J1p] + [Op.L_, Op.U_, Op.L, Op.F, Op.L_, Op.U_, Op.L, Op.U, Op.L, Op.F_, Op.L_, Op.L_, Op.U, Op.L, Op.U]
    } else if  r5 == r6 && o5 == o7 && o7 == b6 && r7 != r6 { // G2
        return [Op.G2p] + [Op.F_, Op.U_, Op.F, Op.R, Op.R, Op.D, Op.Y, Op.R_, Op.U, Op.R, Op.U_, Op.R, Op.D_, Op.Y_, Op.R, Op.R]
    } else if  r7 == r6 && o5 == o7 && o5 == g6 && r5 == b6 { // G4
        return [Op.G4p] + [Op.F, Op.U, Op.F_, Op.L_, Op.L_, Op.D_, Op.Y_, Op.L, Op.U_, Op.L_, Op.U, Op.L_, Op.D, Op.Y, Op.L_, Op.L_]
    } else if  r7 == b5 && r5 == b6 && r6 == b7 && g6 == g5 { // F
        return [Op.Fp] + [Op.R_, Op.U_, Op.F_, Op.R, Op.U, Op.R_, Op.U_, Op.R_, Op.F, Op.R, Op.R, Op.U_, Op.R_, Op.U_, Op.R, Op.U, Op.R_, Op.U, Op.R]
    } else if  r6 == r7 && g5 == g6 && g5 == b7 && r7 == o5 { // V
        return [Op.Vp] + [Op.R_, Op.U, Op.R_, Op.U_, Op.Y, Op.R_, Op.F_, Op.R, Op.R, Op.U_, Op.R_, Op.U, Op.R_, Op.F, Op.R, Op.F, Op.Y_]
    } else if  r7 == r6 && b7 == b6 && o7 == o6 && o6 == r5 && b6 != b5 && g5 != g6 { // N2
        return [Op.N2p] + [Op.R_, Op.U, Op.R, Op.U_, Op.R_, Op.F_, Op.U_, Op.F, Op.R, Op.U, Op.R_, Op.F, Op.R_, Op.F_, Op.R, Op.U_, Op.R]
    } else if  r5 == r6 && b5 == b6 && o5 == o6 && o6 == r7 && b6 != b7 { // N1
        return [Op.N1p] + [Op.L, Op.U_, Op.L_, Op.U, Op.L, Op.F, Op.U, Op.F_, Op.L_, Op.U_, Op.L, Op.F_, Op.L, Op.F, Op.L_, Op.U, Op.L_]
    } else if  r5 == b6 && b6 == o7 && r7 == g6 && g6 == o5 { // E
        return [Op.Ep] + [Op.R, Op.B_, Op.R_, Op.F, Op.R, Op.B, Op.R_, Op.F_, Op.R, Op.B, Op.R_, Op.F, Op.R, Op.B_, Op.R_, Op.F_]
    } else { return []
    }
}


func prSeq(_ ops_arg: [Op]) -> String {
    if ops_arg.count == 0 { return "" }

    var xs = ops_arg
    let x = xs.remove(at: 0)

    if x == Op.FstLayer { return "First Layer -----\n " + prSeq(xs)
    } else if x == Op.SndLayer { return "\nSecond Layer -----\n " + prSeq(xs)
    } else if x == Op.PLL1p
                || x == Op.PLL21p
                || x == Op.PLL22p
                || x == Op.PLL23p 
                || x == Op.PLL24p
                || x == Op.PLL25p 
                || x == Op.PLL26p { return "\nPLL -----\n " + prSeq(xs)
    } else if x == Op.A2p 
                || x == Op.A1p 
                || x == Op.Tp 
                || x == Op.U1p 
                || x == Op.U2p
                || x == Op.Yp
                || x == Op.R2p
                || x == Op.Zp 
                || x == Op.R1p
                || x == Op.Hp 
                || x == Op.G2p
                || x == Op.J2p
                || x == Op.G4p
                || x == Op.J1p
                || x == Op.G1p
                || x == Op.G3p
                || x == Op.Fp
                || x == Op.Vp 
                || x == Op.N2p
                || x == Op.N1p
                || x == Op.Ep { return "\nOLL -----\n " + prSeq(xs)
//    } else { return op2s(x) + " " + prSeq(xs)
    } else { return x.rawValue + " " + prSeq(xs)
    }
}



class Surface {
    var c1, c2, c3, c4, c5, c6, c7, c8: Color
    
    init(_ c1: Color, _ c2: Color, _ c3: Color, _ c4: Color,
         _ c5: Color, _ c6: Color, _ c7: Color, _ c8: Color) {
        self.c1 = c1
        self.c2 = c2
        self.c3 = c3
        self.c4 = c4
        self.c5 = c5
        self.c6 = c6
        self.c7 = c7
        self.c8 = c8
    }

    func getColor(_ num: Int) -> Color {
        switch num {
        case 1: return c1
        case 2: return c2
        case 3: return c3
        case 4: return c4
        case 5: return c5
        case 6: return c6
        case 7: return c7
        case 8: return c8
        default : return c1 // error
        }
    }

    func check(_ c: Color) -> Bool {
        return c1 == c && c2 == c && c3 == c &&
          c4 == c && c5 == c && c6 == c
    }

    func dupSurface() -> Surface {
        return Surface(c1, c2, c3, c4, c5, c6, c7, c8)
    }
}

class Cube {
    var w, r, b, o, g, y: Surface
    
    init(w: Surface, r: Surface, b: Surface, o: Surface, g: Surface, y: Surface) {
        self.w = w
        self.r = r
        self.b = b
        self.o = o
        self.g = g
        self.y = y
    }
    
    func pr() -> String {
        let fw = {self.w.getColor($0).rawValue}
        let fr = {self.r.getColor($0).rawValue}
        let fb = {self.b.getColor($0).rawValue}
        let fo = {self.o.getColor($0).rawValue}
        let fg = {self.g.getColor($0).rawValue}
        let fy = {self.y.getColor($0).rawValue}
        let w765 = "\(fw(7))\(fw(6))\(fw(5))"
        let w8_4 = "\(fw(8))W\(fw(4))"
        let w123 = "\(fw(1))\(fw(2))\(fw(3))"
        let r765 = "\(fr(7))\(fr(6))\(fr(5))"
        let r8_4 = "\(fr(8))R\(fr(4))"
        let r123 = "\(fr(1))\(fr(2))\(fr(3))"
        let b765 = "\(fb(7))\(fb(6))\(fb(5))"
        let b8_4 = "\(fb(8))B\(fb(4))"
        let b123 = "\(fb(1))\(fb(2))\(fb(3))"
        let o765 = "\(fo(7))\(fo(6))\(fo(5))"
        let o8_4 = "\(fo(8))O\(fo(4))"
        let o123 = "\(fo(1))\(fo(2))\(fo(3))"
        let g765 = "\(fg(7))\(fg(6))\(fg(5))"
        let g8_4 = "\(fg(8))G\(fg(4))"
        let g123 = "\(fg(1))\(fg(2))\(fg(3))"
        let y765 = "\(fy(7))\(fy(6))\(fy(5))"
        let y8_4 = "\(fy(8))Y\(fy(4))"
        let y123 = "\(fy(1))\(fy(2))\(fy(3))"
        let spc = "   "
        return spc + " " + w765 + "\n"
            + spc + " " + w8_4 + "\n"
            + spc + " " + w123 + "\n"
            + g765 + " " + r765 + " " + b765 + " " + o765 + "\n"
            + g8_4 + " " + r8_4 + " " + b8_4 + " " + o8_4 + "\n"
            + g123 + " " + r123 + " " + b123 + " " + o123 + "\n"
            + spc + " " + y765 + "\n"
            + spc + " " + y8_4 + "\n"
            + spc + " " + y123 + "\n"
    }

    func dupCube() -> Cube {
        return Cube(w: w.dupSurface(), r: r.dupSurface(), b: b.dupSurface(),
                    o: o.dupSurface(), g: g.dupSurface(), y: y.dupSurface())
    }
    
    func check() -> Bool {
        return w.check(Color.White) && r.check(Color.Red) && b.check(Color.Blue) &&
          o.check(Color.Orange) && g.check(Color.Green) && y.check(Color.Yellow)
    }

    func turn(_ op:Op) -> Cube {
        let oldCube = dupCube()
        let (or, oy, oo, ow, ob, og) =
          (oldCube.r, oldCube.y, oldCube.o, oldCube.w, oldCube.b, oldCube.g)
        switch op {
        case Op.R:
            (w.c3, w.c4, w.c5) = (or.c3, or.c4, or.c5)
            (r.c3, r.c4, r.c5) = (oy.c3, oy.c4, oy.c5)
            (y.c3, y.c4, y.c5) = (oo.c7, oo.c8, oo.c1)
            (o.c1, o.c8, o.c7) = (ow.c5, ow.c4, ow.c3)
            (b.c1, b.c2, b.c3, b.c4) = (ob.c3, ob.c4, ob.c5, ob.c6)
            (b.c5, b.c6, b.c7, b.c8) = (ob.c7, ob.c8, ob.c1, ob.c2)
            return self
        case Op.Y:
            (w.c1, w.c2, w.c3, w.c4) = (ow.c3, ow.c4, ow.c5, ow.c6)
            (w.c5, w.c6, w.c7, w.c8) = (ow.c7, ow.c8, ow.c1, ow.c2)
            (r.c1, r.c2, r.c3, r.c4) = (ob.c1, ob.c2, ob.c3, ob.c4)
            (r.c5, r.c6, r.c7, r.c8) = (ob.c5, ob.c6, ob.c7, ob.c8)
            (b.c1, b.c2, b.c3, b.c4) = (oo.c1, oo.c2, oo.c3, oo.c4)
            (b.c5, b.c6, b.c7, b.c8) = (oo.c5, oo.c6, oo.c7, oo.c8)
            (o.c1, o.c2, o.c3, o.c4) = (og.c1, og.c2, og.c3, og.c4)
            (o.c5, o.c6, o.c7, o.c8) = (og.c5, og.c6, og.c7, og.c8)
            (g.c1, g.c2, g.c3, g.c4) = (or.c1, or.c2, or.c3, or.c4)
            (g.c5, g.c6, g.c7, g.c8) = (or.c5, or.c6, or.c7, or.c8)
            (y.c1, y.c2, y.c3, y.c4) = (oy.c7, oy.c8, oy.c1, oy.c2)
            (y.c5, y.c6, y.c7, y.c8) = (oy.c3, oy.c4, oy.c5, oy.c6)
            return self
        case Op.Z:
            (w.c1, w.c2, w.c3, w.c4) = (og.c3, og.c4, og.c5, og.c6)
            (w.c5, w.c6, w.c7, w.c8) = (og.c7, og.c8, og.c1, og.c2)
            (r.c1, r.c2, r.c3, r.c4) = (or.c3, or.c4, or.c5, or.c6)
            (r.c5, r.c6, r.c7, r.c8) = (or.c7, or.c8, or.c1, or.c2)
            (b.c1, b.c2, b.c3, b.c4) = (ow.c3, ow.c4, ow.c5, ow.c6)
            (b.c5, b.c6, b.c7, b.c8) = (ow.c7, ow.c8, ow.c1, ow.c2)
            (o.c1, o.c2, o.c3, o.c4) = (oo.c7, oo.c8, oo.c1, oo.c2)
            (o.c5, o.c6, o.c7, o.c8) = (oo.c3, oo.c4, oo.c5, oo.c6)
            (g.c1, g.c2, g.c3, g.c4) = (oy.c3, oy.c4, oy.c5, oy.c6)
            (g.c5, g.c6, g.c7, g.c8) = (oy.c7, oy.c8, oy.c1, oy.c2)
            (y.c1, y.c2, y.c3, y.c4) = (ob.c3, ob.c4, ob.c5, ob.c6)
            (y.c5, y.c6, y.c7, y.c8) = (ob.c7, ob.c8, ob.c1, ob.c2)
            return self
        case Op.R_: return self.turn(Op.R).turn(Op.R).turn(Op.R)
        case Op.R2: return self.turn(Op.R).turn(Op.R)
        case Op.Y_: return self.turn(Op.Y).turn(Op.Y).turn(Op.Y)
        case Op.Z2: return self.turn(Op.Z).turn(Op.Z)
        case Op.Z_: return self.turn(Op.Z).turn(Op.Z).turn(Op.Z)
        case Op.L: return self.applySeq([Op.Y, Op.Y, Op.R, Op.Y, Op.Y])
        case Op.L2: return self.turn(Op.L).turn(Op.L)
        case Op.L_: return self.applySeq([Op.Y, Op.Y, Op.R_, Op.Y, Op.Y])
        case Op.U: return self.turn(Op.Z).turn(Op.R).turn(Op.Z_)
        case Op.U2: return self.turn(Op.U).turn(Op.U)
        case Op.U_: return self.turn(Op.Z).turn(Op.R_).turn(Op.Z_)
        case Op.D: return self.turn(Op.Z_).turn(Op.R).turn(Op.Z)
        case Op.D2: return self.turn(Op.D).turn(Op.D)
        case Op.D_: return self.turn(Op.Z_).turn(Op.R_).turn(Op.Z)
        case Op.F: return self.turn(Op.Y_).turn(Op.R).turn(Op.Y)
        case Op.F2: return self.turn(Op.F).turn(Op.F)
        case Op.F_: return self.turn(Op.Y_).turn(Op.R_).turn(Op.Y)
        case Op.B: return self.turn(Op.Y).turn(Op.R).turn(Op.Y_)
        case Op.B2: return self.turn(Op.B).turn(Op.B)
        case Op.B_: return self.turn(Op.Y).turn(Op.R_).turn(Op.Y_)
        case Op.Y2: return self.turn(Op.Y).turn(Op.Y)
        case Op.Y_2: return self.turn(Op.Y_).turn(Op.Y_)
        case Op.Z_2: return self.turn(Op.Z_).turn(Op.Z_)
        case Op.U_2: return self.turn(Op.U_).turn(Op.U_)
        case Op.B_2: return self.turn(Op.B_).turn(Op.B_)
        case Op.D_2: return self.turn(Op.D_).turn(Op.D_)
        case Op.L_2: return self.turn(Op.L_).turn(Op.L_)
        case Op.R_2: return self.turn(Op.R_).turn(Op.R_)
        case Op.F_2: return self.turn(Op.F_).turn(Op.F_)
        case Op.M: return self.applySeq([Op.Z, Op.D_, Op.U, Op.Y_, Op.Z_])
        default: break
        }
        return self
    }

    func turn2(_ q:Cube, _ op:Op) -> Cube {
        return q.turn(op)
    }

    func applySeq(_ l:[Op]) -> Cube {
        return l.reduce(self, turn2)
    }
}


let goal = Cube(w: Surface(s2c("W"),s2c("W"),s2c("W"),s2c("W"),
                           s2c("W"),s2c("W"),s2c("W"),s2c("W")),
                r: Surface(s2c("R"),s2c("R"),s2c("R"),s2c("R"),
                           s2c("R"),s2c("R"),s2c("R"),s2c("R")),
                b: Surface(s2c("B"),s2c("B"),s2c("B"),s2c("B"),
                           s2c("B"),s2c("B"),s2c("B"),s2c("B")),
                o: Surface(s2c("O"),s2c("O"),s2c("O"),s2c("O"),
                           s2c("O"),s2c("O"),s2c("O"),s2c("O")),
                g: Surface(s2c("G"),s2c("G"),s2c("G"),s2c("G"),
                           s2c("G"),s2c("G"),s2c("G"),s2c("G")),
                y: Surface(s2c("Y"),s2c("Y"),s2c("Y"),s2c("Y"),
                           s2c("Y"),s2c("Y"),s2c("Y"),s2c("Y")))


func solve(_ str: String) -> [Op] {
    return solveQ(goal.dupCube().applySeq(fromString(str)))
}

func solve_check(_ str: String) {
    let ins = fromString(str)
    let outs = solve(str)
    let q_start = goal.dupCube().applySeq(ins)
    print("Scramble:")
    print(str)
//    print(ins)
    print("Scrambled:")
    print(q_start.pr())
    print("Solution:")
//    print(outs)
    print(prSeq(outs))
    print("Solved:")
    print(q_start.dupCube().applySeq(outs).pr())
}

func solve_check_pat(_ str: String) {
    let q_start = fromStringPos(str)
    let outs = solveQ(q_start)
    print("Scrambled:")
    print(q_start.pr())
    print("Solution:")
//    print(outs)
    print(prSeq(outs))
    print("Solved:")
    print(q_start.dupCube().applySeq(outs).pr())
}

/**************************************************************/

print("Input a scramble:")
let ins = readLine(strippingNewline:true)
if let ins2 = ins {
    solve_check(ins2)
}

/*
print("Input a scrambled pattern:")
let pat = readLine(strippingNewline:true)
if let pat2 = pat {
    solve_check_pat(pat2)
}
*/
