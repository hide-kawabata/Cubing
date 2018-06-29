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

enum CubingError : Error {
    case ParseError
}

// helper functions
func s2c(_ c: String) -> Color {
    switch c {
    case "W": return .White
    case "R": return .Red
    case "B": return .Blue
    case "O": return .Orange
    case "G": return .Green
    case "Y": return .Yellow
    default: return .White // error s2c
    }
}


func s2op(_ s: String) -> Op {
    switch s {
    case "R": return .R
    case "R'": return .R_
    case "U": return .U
    case "U'": return .U_
    case "B": return .B
    case "B'": return .B_
    case "L": return .L
    case "L'": return .L_
    case "F": return .F
    case "F'": return .F_
    case "D": return .D
    case "D'": return .D_
    case "Y": return .Y
    case "Y'": return .Y_
    case "Z": return .Z
    case "Z'": return .Z_
    case "N": return .N
    case "M": return .M
    case "M'": return .M_
    case "Y2": return .Y2
    case "Y'2": return .Y_2
    case "Z2": return .Z2
    case "Z'2": return .Z_2
    case "R2": return .R2
    case "R'2": return .R_2
    case "U2": return .U2
    case "U'2": return .U_2
    case "B2": return .B2
    case "B'2": return .B_2
    case "L2": return .L2
    case "L'2": return .L_2
    case "F2": return .F2
    case "F'2": return .F_2
    case "D2": return .D2
    case "D'2": return .D_2
//
    case "FstLayer": return .FstLayer
    case "SndLayer": return .SndLayer
    case "PLL1p": return .PLL1p
    case "PLL21p": return .PLL21p
    case "PLL22p": return .PLL22p
    case "PLL23p": return .PLL23p
    case "PLL24p": return .PLL24p
    case "PLL25p": return .PLL25p
    case "PLL26p": return .PLL26p
    case "A2p": return .A2p
    case "A1p": return .A1p
    case "Tp": return .Tp
    case "U1p": return .U1p
    case "U2p": return .U2p
    case "Yp": return .Yp
    case "R2p": return .R2p
    case "Zp": return .Zp
    case "R1p": return .R1p
    case "Hp": return .Hp
    case "G2p": return .G2p
    case "J2p": return .J2p
    case "G4p": return .G4p
    case "J1p": return .J1p
    case "G1p": return .G1p
    case "G3p": return .G3p
    case "Fp": return .Fp
    case "Vp": return .Vp
    case "N2p": return .N2p
    case "N1p": return .N1p
    case "Ep": return .Ep
    default: return .N // error s2op
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
    let sw = collist[0].map({s2c($0)})
    let sr = collist[1].map({s2c($0)})
    let sb = collist[2].map({s2c($0)})
    let so = collist[3].map({s2c($0)})
    let sg = collist[4].map({s2c($0)})
    let sy = collist[5].map({s2c($0)})
    return Cube(w: Surface(sw), r: Surface(sr), b: Surface(sb),
                o: Surface(so), g: Surface(sg), y: Surface(sy))
}


func readSurface(_ q: Cube) -> [Op]? {
    func checkSurface(_ q: Cube, _ colors: (Color, Color, Color, Color, 
                                            Color, Color, Color, Color)) -> Op {
        switch colors {
        case (q.r.c1, q.r.c2, q.y.c3, q.y.c4, q.y.c5, q.r.c6, q.r.c7, q.r.c8): return .R
        case (q.w.c1, q.w.c2, q.r.c3, q.r.c4, q.r.c5, q.w.c6, q.w.c7, q.w.c8): return .R
        case (q.o.c5, q.o.c6, q.w.c3, q.w.c4, q.w.c5, q.o.c2, q.o.c3, q.o.c4): return .R
        case (q.y.c1, q.y.c2, q.o.c7, q.o.c8, q.o.c1, q.y.c6, q.y.c7, q.y.c8): return .R
        case (q.r.c1, q.r.c2, q.w.c3, q.w.c4, q.w.c5, q.r.c6, q.r.c7, q.r.c8): return .R_
        case (q.w.c1, q.w.c2, q.o.c7, q.o.c8, q.o.c1, q.w.c6, q.w.c7, q.w.c8): return .R_
        case (q.o.c5, q.o.c6, q.y.c3, q.y.c4, q.y.c5, q.o.c2, q.o.c3, q.o.c4): return .R_
        case (q.y.c1, q.y.c2, q.r.c3, q.r.c4, q.r.c5, q.y.c6, q.y.c7, q.y.c8): return .R_
        default: return .N
        }
    }

    func oneSurface(_ str: String) -> [Op] {
        let cs = str.characters.map({s2c("\($0)")}) // length: 8
        let colors = (cs[0], cs[1], cs[2], cs[3], cs[4], cs[5], cs[6], cs[7])
        let op1 = checkSurface(q, colors) // R, R'
        let op2 = checkSurface(q.dupCube().turn(.Y), colors) // B, B'
        let op3 = checkSurface(q.dupCube().turn(.Y).turn(.Y), colors) // L, L'
        let op4 = checkSurface(q.dupCube().turn(.Y_), colors) // F, F'
        let op5 = checkSurface(q.dupCube().turn(.Z), colors) // U, U'
        let op6 = checkSurface(q.dupCube().turn(.Z_), colors) // D, D'
        return [op1, op2, op3, op4, op5, op6]
    }

    func checkAmbiguity(_ l: [Op]) -> Bool { // ambiguous=T
        print(l)
        if l.map({(x:Op) -> Int in if x != .N { return 1 } else { return 0 }})
             .reduce(0, {$0 + $1})
             > 1 {
            print("Ambiguous")
            return true
        } else {
            print("Not ambiguous")
            return false
        }
    }

    func mergeOps(_ l1: [Op], _ l2: [Op], _ acc: [Op]) -> [Op] {
        if l1.count == 0 { return acc }
        var l1t = l1
        var l2t = l2
        let l1h = l1t.remove(at: 0)
        let l2h = l2t.remove(at: 0)
        if l1h == .N || l2h == .N {
            return mergeOps(l1t, l2t, acc + [.N]) 
        } else {
            return mergeOps(l1t, l2t, acc + [l1h])
        }
    }

    var pats:[Op] = [.R, .R, .R, .R, .R, .R] // initial dummy value
    var ambiguous = true
    var i = 1
    while ambiguous && i <= 3 {
        print("trial no." + String(i))
        i = i + 1
        let sur = readLine(strippingNewline:true)
        if let sur2 = sur {
            let ops = oneSurface(sur2)
            pats = mergeOps(pats, ops, [])
            ambiguous = checkAmbiguity(pats)
        } else {
            continue
        }
    }

    if i >= 3 {
        print("Please input whole configuration")
        return nil
    } else {
        if pats[0] != .N {
            return [pats[0]]
        } else if pats[1] != .N {
            return [.Y, pats[1], .Y_]
        } else if pats[2] != .N {
            return [.Y, .Y, pats[2], .Y_, .Y_]
        } else if pats[3] != .N {
            return [.Y_, pats[3], .Y]
        } else if pats[4] != .N {
            return [.Z, pats[4], .Z_]
        } else if pats[5] != .N {
            return [.Z_, pats[5], .Z]
        } else {
            return nil
        }
    }
}

func revOp(_ x: Op) -> Op {
    switch x {
    case .U: return .U_
    case .L: return .L_
    case .R: return .R_
    case .B: return .B_
    case .D: return .D_
    case .F: return .F_
    case .Y: return .Y_
    case .Z: return .Z_
    case .U_: return .U
    case .L_: return .L
    case .R_: return .R
    case .B_: return .B
    case .D_: return .D
    case .F_: return .F
    case .Y_: return .Y
    case .Z_: return .Z
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
    case .R2: return [.R, .R] + expandOp(xs)
    case .L2: return [.L, .L] + expandOp(xs)
    case .D2: return [.D, .D] + expandOp(xs)
    case .U2: return [.U, .U] + expandOp(xs)
    case .B2: return [.B, .B] + expandOp(xs)
    case .F2: return [.F, .F] + expandOp(xs)
    case .Y2: return [.Y, .Y] + expandOp(xs)
    case .Z2: return [.Z, .Z] + expandOp(xs)
    case .R_2: return [.R, .R] + expandOp(xs)
    case .L_2: return [.L, .L] + expandOp(xs)
    case .D_2: return [.D, .D] + expandOp(xs)
    case .U_2: return [.U, .U] + expandOp(xs)
    case .B_2: return [.B, .B] + expandOp(xs)
    case .F_2: return [.F, .F] + expandOp(xs)
    case .Y_2: return [.Y, .Y] + expandOp(xs)
    case .Z_2: return [.Z, .Z] + expandOp(xs)
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
    case (.Y, .U): return [y] + exchangeOp([x] + xs)
    case (.Y, .U_): return [y] + exchangeOp([x] + xs)
    case (.Y_, .U): return [y] + exchangeOp([x] + xs)
    case (.Y_, .U_): return [y] + exchangeOp([x] + xs)
    case (.Y, .D): return [y] + exchangeOp([x] + xs)
    case (.Y, .D_): return [y] + exchangeOp([x] + xs)
    case (.Y_, .D): return [y] + exchangeOp([x] + xs)
    case (.Y_, .D_): return [y] + exchangeOp([x] + xs)
                       // D
    case (.D, .U): return [y] + exchangeOp([x] + xs)
    case (.D, .U_): return [y] + exchangeOp([x] + xs)
    case (.D_, .U): return [y] + exchangeOp([x] + xs)
    case (.D_, .U_): return [y] + exchangeOp([x] + xs)
                       // B
    case (.B, .F): return [y] + exchangeOp([x] + xs)
    case (.B, .F_): return [y] + exchangeOp([x] + xs)
    case (.B_, .F): return [y] + exchangeOp([x] + xs)
    case (.B_, .F_): return [y] + exchangeOp([x] + xs)
                       // L
    case (.L, .R): return [y] + exchangeOp([x] + xs)
    case (.L, .R_): return [y] + exchangeOp([x] + xs)
    case (.L_, .R): return [y] + exchangeOp([x] + xs)
    case (.L_, .R_): return [y] + exchangeOp([x] + xs)
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
    case (.U, .U): return [.U2] + mergeOp(ys)
    case (.B, .B): return [.B2] + mergeOp(ys)
    case (.F, .F): return [.F2] + mergeOp(ys)
    case (.D, .D): return [.D2] + mergeOp(ys)
    case (.L, .L): return [.L2] + mergeOp(ys)
    case (.R, .R): return [.R2] + mergeOp(ys)
    case (.Y, .Y): return [.Y2] + mergeOp(ys)
    case (.Z, .Z): return [.Z2] + mergeOp(ys)
    case (.U_, .U_): return [.U2] + mergeOp(ys)
    case (.B_, .B_): return [.B2] + mergeOp(ys)
    case (.F_, .F_): return [.F2] + mergeOp(ys)
    case (.D_, .D_): return [.D2] + mergeOp(ys)
    case (.L_, .L_): return [.L2] + mergeOp(ys)
    case (.R_, .R_): return [.R2] + mergeOp(ys)
    case (.Y_, .Y_): return [.Y2] + mergeOp(ys)
    case (.Z_, .Z_): return [.Z2] + mergeOp(ys)
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
    pair = step(pair, setRY(.N))
    pair = step(pair, {[.Y] + setRY(.Y)($0.dupCube().turn(.Y)) + [.Y_]})
    pair = step(pair, {[.Y2] + setRY(.Y2)($0.dupCube().turn(.Y2)) + [.Y2]})
    pair = step(pair, {[.Y_] + setRY(.Y_)($0.dupCube().turn(.Y_)) + [.Y]})
    pair = step(pair, {(_) in [.SndLayer]})
    pair = step(pair, setYGR(.N))
    pair = step(pair, {[.Y] + setYGR(.Y)($0.dupCube().turn(.Y)) + [.Y_]})
    pair = step(pair, {[.Y2] + setYGR(.Y2)($0.dupCube().turn(.Y2)) + [.Y2]})
    pair = step(pair, {[.Y_] + setYGR(.Y_)($0.dupCube().turn(.Y_)) + [.Y]})
    pair = step(pair, setGR(.N))
    pair = step(pair, {[.Y] + setGR(.Y)($0.dupCube().turn(.Y)) + [.Y_]})
    pair = step(pair, {[.Y2] + setGR(.Y2)($0.dupCube().turn(.Y2)) + [.Y2]})
    pair = step(pair, {[.Y_] + setGR(.Y_)($0.dupCube().turn(.Y_)) + [.Y]})
    pair = step(pair, oneToThree)
    pair = step(pair, threeToFive)
    pair = step(pair, fiveToNine)
    pair = step(pair, {[.Y] + fiveToNine($0.dupCube().turn(.Y)) + [.Y_]})
    pair = step(pair, {[.Y2] + fiveToNine($0.dupCube().turn(.Y2)) + [.Y2]})
    pair = step(pair, {[.Y_] + fiveToNine($0.dupCube().turn(.Y_)) + [.Y]})
    pair = step(pair, nineToFinish)
    pair = step(pair, {[.Y] + nineToFinish($0.dupCube().turn(.Y)) + [.Y_]})
    pair = step(pair, {[.Y2] + nineToFinish($0.dupCube().turn(.Y2)) + [.Y2]})
    pair = step(pair, {[.Y_] + nineToFinish($0.dupCube().turn(.Y_)) + [.Y]})
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
    } else if q.dupCube().turn(.U).check() { return [.U]
    } else if q.dupCube().turn(.U2).check() { return [.U2]
    } else if q.dupCube().turn(.U_).check() { return [.U_]
    } else { return [.U] // error_finishQ
    }
}


func rotc(_ op:Op, _ c:Color) -> Color {
    switch op {
    case .N: return c
    case .Y:
        switch c {
        case .Red: return .Blue
        case .Blue: return .Orange
        case .Orange: return .Green
        case .Green: return .Red
        default: return c
        }
    case .Y2:
        return rotc(.Y, (rotc(.Y, c)))
    case .Y_:
        return rotc(.Y, (rotc(.Y2, c)))
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
    let yellow = rotc(tc, .Yellow)
    let red = rotc(tc, .Red)

    if sr(2) == red && sy(6) == yellow { return []
//
    } else if sr(2) == yellow && sy(6) == red { return [.F_, .D, .R_, .D_]
    } else if sr(4) == yellow && sb(8) == red { return [.D, .R_, .D_]
    } else if sr(4) == red && sb(8) == yellow { return [.F]
    } else if sr(6) == red && sw(2) == yellow { return [.F, .F]
    } else if sr(6) == yellow && sw(2) == red { return [.U_, .R_, .F, .R]
    } else if sr(8) == yellow && sg(4) == red { return [.D_, .L, .D]
    } else if sr(8) == red && sg(4) == yellow { return [.F_]
//
    } else if sb(2) == red && sy(4) == yellow { return [.R, .D, .R_, .D_]
    } else if sb(2) == yellow && sy(4) == red { return [.R, .F]
    } else if sb(4) == yellow && so(8) == red { return [.B, .U, .U, .B_, .F, .F]
    } else if sb(4) == red && so(8) == yellow { return [.R_, .U, .R, .F, .F]
    } else if sb(6) == red && sw(4) == yellow { return [.U, .F, .F]
    } else if sb(6) == yellow && sw(4) == red { return [.R_, .F, .R]
//
    } else if so(2) == red && sy(2) == yellow { return [.B, .B, .U, .U, .F, .F]
    } else if so(2) == yellow && sy(2) == red { return [.B, .B, .U, .R_, .F, .R]
    } else if so(4) == yellow && sg(8) == red { return [.L, .U_, .L_, .F, .F]
    } else if so(4) == red && sg(8) == yellow { return [.B_, .U_, .B, .U_, .F, .F]
    } else if so(6) == red && sw(6) == yellow { return [.U, .U, .F, .F]
    } else if so(6) == yellow && sw(6) == red { return [.U_, .L, .F_, .L_]
//
    } else if sg(2) == red && sy(8) == yellow { return [.L_, .D_, .L, .D]
    } else if sg(2) == yellow && sy(8) == red { return [.L_, .F_]
    } else if sg(6) == red && sw(8) == yellow { return [.U_, .F, .F]
    } else if sg(6) == yellow && sw(8) == red { return [.L, .F_, .L_]
    } else { return [.U] // error_setRY
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
    let yellow = rotc(tc, .Yellow)
    let green = rotc(tc, .Green)
    let red = rotc(tc, .Red)

    if sr(1) == red && sg(3) == green { return []
//
    } else if sr(1) == yellow && sg(3) == red { return [.F, .U, .F_, .U_, .F, .U, .F_]
    } else if sr(1) == green && sg(3) == yellow { return [.L_, .U_, .L, .U, .L_, .U_, .L]
//
    } else if sr(3) == yellow && sb(1) == green { return [.F_, .U_, .F, .U, .U, .L_, .U_, .L]
    } else if sr(3) == red && sb(1) == yellow { return [.R, .U, .R_, .F, .U, .F_]
    } else if sr(3) == green && sb(1) == red { return [.R, .U, .R_, .L_, .U_, .L]
//
    } else if sr(5) == green && sb(7) == yellow { return [.L_, .U, .L]
    } else if sr(5) == yellow && sb(7) == red { return [.U, .L_, .U_, .L]
    } else if sr(5) == red && sb(7) == green { return [.U, .L_, .U, .L, .U_, .U_, .L_, .U_, .L]
//
    } else if sr(7) == red && sg(5) == yellow { return [.L_, .U_, .L]
    } else if sr(7) == green && sg(5) == red { return [.F, .U, .U, .F_, .U_, .F, .U, .F_]
    } else if sr(7) == yellow && sg(5) == green { return [.F, .U, .F_]
//
    } else if sb(3) == green && so(1) == red { return [.R_, .U_, .U_, .R, .F, .U, .F_]
    } else if sb(3) == yellow && so(1) == green { return [.R_, .U_, .R, .U_, .L_, .U_, .L]
    } else if sb(3) == red && so(1) == yellow { return [.B, .U, .B_, .U, .F, .U, .F_]
//
    } else if sb(5) == green && so(7) == yellow { return [.U, .U, .F, .U, .F_]
    } else if sb(5) == red && so(7) == green { return [.U, .U, .L_, .U, .L, .U, .U, .L_, .U_, .L]
    } else if sb(5) == yellow && so(7) == red { return [.U, .U, .L_, .U_, .L]
//
    } else if so(3) == red && sg(1) == yellow { return [.L, .U, .L_, .U, .U, .F, .U, .F_]
    } else if so(3) == yellow && sg(1) == green { return [.B_, .U_, .B, .L_, .U_, .L]
    } else if so(3) == green && sg(1) == red { return [.L, .U_, .L_, .U_, .F, .U, .F_]
//
    } else if so(5) == yellow && sg(7) == red { return [.F, .U_, .F_]
    } else if so(5) == green && sg(7) == yellow { return [.U_, .F, .U, .F_]
    } else if so(5) == red && sg(7) == green { return [.F, .U, .U, .F_, .L_, .U_, .L]
    } else { return [.U] // error_setYGR
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
    let green = rotc(tc, .Green)
    let red = rotc(tc, .Red)

    if sr(8) == red && sg(4) == green { return []
//
    } else if sr(8) == green && sg(4) == red {
        return [.F, .U_, .F_, .U_, .L_, .U, .L, .U_, .F, .U_, .F_, .U_, .L_, .U, .L]
    } else if sr(6) == red && sw(2) == green { return [.U_, .L_, .U, .L, .U, .F, .U_, .F_]
    } else if sr(6) == green && sw(2) == red { return [.U, .U, .F, .U_, .F_, .U_, .L_, .U, .L]
    } else if sr(4) == green && sb(8) == red { 
        return [.R, .U_, .R_, .U_, .F_, .U, .F, .U, .L_, .U, .L, .U, .F, .U_, .F_]
    } else if sr(4) == red && sb(8) == green {
        return [.R, .U_, .R_, .U_, .F_, .U, .F, .F, .U_, .F_, .U_, .L_, .U, .L]
//
    } else if sb(4) == green && so(8) == red {
        return [.Y, .R, .U_, .R_, .U_, .F_, .U, .F, .Y_, .U, .U, .L_, .U, .L, .U, .F, .U_, .F_]
    } else if sb(4) == red && so(8) == green {
        return [.Y, .R, .U_, .R_, .U_, .F_, .U, .F, .Y_, .U, .F, .U_, .F_, .U_, .L_, .U, .L]
    } else if sb(6) == red && sw(4) == green { return [.L_, .U, .L, .U, .F, .U_, .F_]
    } else if sb(6) == green && sw(4) == red { return [.U_, .F, .U_, .F_, .U_, .L_, .U, .L]
//
    } else if so(6) == red && sw(6) == green { return [.U, .L_, .U, .L, .U, .F, .U_, .F_]
    } else if so(6) == green && sw(6) == red { return [.F, .U_, .F_, .U_, .L_, .U, .L]
    } else if so(4) == red && sg(8) == green {
        return [.Y, .Y, .R, .U_, .R_, .U_, .F_, .U, .F, .Y, .Y, .U, .U, .F, .U_, .F_, .U_, .L_, .U, .L]
    } else if so(4) == green && sg(8) == red {
        return [.Y, .Y, .R, .U_, .R_, .U_, .F_, .U, .F, .Y, .Y, .U_, .L_, .U, .L, .U, .F, .U_, .F_]
//
    } else if sg(6) == red && sw(8) == green { return [.U, .U, .L_, .U, .L, .U, .F, .U_, .F_]
    } else if sg(6) == green && sw(8) == red { return [.U, .F, .U_, .F_, .U_, .L_, .U, .L]
    } else { return [.U] // error_setGR
    }
  }
  return setGR2
}

// PLL1
func oneToThree(_ q: Cube) -> [Op] {
    let swq = q.w.getColor
    if swq(2) != .White && swq(4) != .White &&
         swq(6) != .White && swq(8) != .White { 
        return [.PLL1p] + [.F, .R, .U, .R_, .U_, .F_]
    } else { return []
    }
}

// PLL2
func threeToFive(_ q: Cube) -> [Op] {
    let sq = q.w.getColor
    let (w2, w4, w6, w8) = (sq(2), sq(4), sq(6), sq(8))
    
    if w2 == w4 && w2 != w6 && w2 != w8 { return [.PLL21p] + [.B, .U, .L, .U_, .L_, .B_]
    } else if  w4 == w6 && w4 != w8 && w4 != w2 { return [.PLL22p] + [.U, .B, .U, .L, .U_, .L_, .B_]
    } else if  w6 == w8 && w6 != w2 && w6 != w4 { return [.PLL23p] + [.U, .U, .B, .U, .L, .U_, .L_, .B_]
    } else if  w8 == w2 && w8 != w4 && w8 != w6 { return [.PLL24p] + [.U_, .B, .U, .L, .U_, .L_, .B_]
    } else if  w4 == w8 && w4 != w2 && w4 != w6 { return [.PLL25p] + [.F, .R, .U, .R_, .U_, .F_]
    } else if  w2 == w6 && w2 != w4 && w2 != w8 { return [.PLL26p] + [.U, .F, .R, .U, .R_, .U_, .F_]
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
        return [.R, .U, .U, .R_, .R_, .U_, .R, .R, .U_, .R_, .R_, .U, .U, .R]
    } else if  w1 == w3 && w3 == o5 && o5 == o7 {
        return [.R, .R, .D_, .R, .U, .U, .R_, .D, .R, .U, .U, .R]
    } else if  r5 == b5 && b5 == o5 && o5 == w1 { return [.R, .U, .R_, .U, .R, .U_, .U_, .R_]
    } else if  r7 == b7 && b7 == g7 && g7 == w5 { return [.R, .U_, .U_, .R_, .U_, .R, .U_, .R_]
    } else if  r7 == r5 && r5 == o7 && o7 == o5 {
        return [.R, .U_, .U_, .R_, .U_, .R, .U, .R_, .U_, .R, .U_, .R_]
    } else if  r7 == b5 && b5 == w3 && w3 == w7 {
        return [.L, .F_, .F_, .R_, .R_, .D, .R, .D_, .R, .F_, .F_, .L_]
    } else if  r7 == w3 && w3 == w5 && w5 == o5 { return [.L, .F, .R_, .F_, .L_, .F, .R, .F_]
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
        return [.A1p] + [.R, .R, .F, .F, .R_, .B_, .R, .F, .F, .R_, .B, .R_]
    } else if  g6 == g7 && o5 == o6 && r5 == r7 && r7 == b6 { // A2
        return [.A2p] + [.R, .B_, .R, .F, .F, .R_, .B, .R, .F, .F, .R_, .R_]
    } else if  r6 == r7 && g5 == g7 && g5 != g6 && o5 == o6 { // T
        return [.Tp] + [.R, .U, .R_, .U_, .R_, .F, .R, .R, .U_, .R_, .U_, .R, .U, .R_, .F_]
    } else if  o5 == o6 && o5 == o7 && r5 != r6 && r5 == r7 { 
        if r5 == b6 {
            return [.U1p] + [.R, .R, .U, .R, .U, .R_, .U_, .R_, .U_, .R_, .U, .R_] // U1
        } else {
            return [.U2p] + [.R, .U_, .R, .U, .R, .U, .R, .U_, .R_, .U_, .R_, .R_] // U2
        }
    } else if  r6 == r7 && b5 == b6 && r7 == o5 && b6 != b7 { // Y
        return [.Yp] + [.F, .R, .U_, .R_, .U_, .R, .U, .R_, .F_, .R, .U, .R_, .U_, .R_, .F, .R, .F_]
    } else if  r5 == r7 && r5 == b6 && r6 == b7 && g5 == g6 { // R2
        return [.R2p] + [.R_, .U_, .U_, .R, .U_, .U_, .R_, .F, .R, .U, .R_, .U_, .R_, .F_, .R, .R, .U_]
    } else if  r5 == r7 && r5 == b6 && r6 == b7 && b5 == b7 { // Z
        return [.Zp] + [.R_, .L, .F_, .R, .R, .L_, .L_, .B_, .R, .R, .L_, .L_, .F_, .R_, .L, .D, .D, .R, .R, .L_, .L_, .U]
    } else if  r5 == r7 && r5 == g6 && r6 == g5 && g7 != g5 { // R1
        return [.R1p] + [.L, .U, .U, .L_, .U, .U, .L, .F_, .L_, .U_, .L, .U, .L, .F, .L_, .L_, .U]
    } else if  r5 == r7 && b5 == b7 && r5 == o6 && b5 == g6 { // H
        return [.Hp] + [.M, .M, .U_, .M, .M, .U_, .U_, .M, .M, .U_, .M, .M]
    } else if  r5 == r6 && g5 == g7 && g7 == b6 && b7 == g6 { // G1
        return [.G1p] + [.R, .R, .D, .Y, .R_, .U, .R_, .U_, .R, .D_, .Y_, .R_, .R_, .F_, .U, .F]
    } else if  r5 == r6 && r6 == o7 && g5 == g6 && g6 == g7 { // J2
        return [.J2p] + [.R, .U, .R_, .F_, .R, .U, .R_, .U_, .R_, .F, .R, .R, .U_, .R_, .U_]
    } else if  r7 == r6 && r6 == o5 && b5 == b7 && b7 == g6 { // G3
        return [.G3p] + [.L_, .L_, .D_, .Y_, .L, .U_, .L, .U, .L_, .D, .Y, .L, .L, .F, .U_, .F_]
    } else if  r7 == r6 && r6 == o5 && b5 == b6 && b6 == b7 { // J1
        return [.J1p] + [.L_, .U_, .L, .F, .L_, .U_, .L, .U, .L, .F_, .L_, .L_, .U, .L, .U]
    } else if  r5 == r6 && o5 == o7 && o7 == b6 && r7 != r6 { // G2
        return [.G2p] + [.F_, .U_, .F, .R, .R, .D, .Y, .R_, .U, .R, .U_, .R, .D_, .Y_, .R, .R]
    } else if  r7 == r6 && o5 == o7 && o5 == g6 && r5 == b6 { // G4
        return [.G4p] + [.F, .U, .F_, .L_, .L_, .D_, .Y_, .L, .U_, .L_, .U, .L_, .D, .Y, .L_, .L_]
    } else if  r7 == b5 && r5 == b6 && r6 == b7 && g6 == g5 { // F
        return [.Fp] + [.R_, .U_, .F_, .R, .U, .R_, .U_, .R_, .F, .R, .R, .U_, .R_, .U_, .R, .U, .R_, .U, .R]
    } else if  r6 == r7 && g5 == g6 && g5 == b7 && r7 == o5 { // V
        return [.Vp] + [.R_, .U, .R_, .U_, .Y, .R_, .F_, .R, .R, .U_, .R_, .U, .R_, .F, .R, .F, .Y_]
    } else if  r7 == r6 && b7 == b6 && o7 == o6 && o6 == r5 && b6 != b5 && g5 != g6 { // N2
        return [.N2p] + [.R_, .U, .R, .U_, .R_, .F_, .U_, .F, .R, .U, .R_, .F, .R_, .F_, .R, .U_, .R]
    } else if  r5 == r6 && b5 == b6 && o5 == o6 && o6 == r7 && b6 != b7 { // N1
        return [.N1p] + [.L, .U_, .L_, .U, .L, .F, .U, .F_, .L_, .U_, .L, .F_, .L, .F, .L_, .U, .L_]
    } else if  r5 == b6 && b6 == o7 && r7 == g6 && g6 == o5 { // E
        return [.Ep] + [.R, .B_, .R_, .F, .R, .B, .R_, .F_, .R, .B, .R_, .F, .R, .B_, .R_, .F_]
    } else { return []
    }
}


func prSeq(_ ops_arg: [Op]) -> String {
    if ops_arg.count == 0 { return "" }

    var xs = ops_arg
    let x = xs.remove(at: 0)

    if x == .FstLayer { return "First Layer -----\n " + prSeq(xs)
    } else if x == .SndLayer { return "\nSecond Layer -----\n " + prSeq(xs)
    } else if x == .PLL1p
                || x == .PLL21p
                || x == .PLL22p
                || x == .PLL23p 
                || x == .PLL24p
                || x == .PLL25p 
                || x == .PLL26p { return "\nPLL -----\n " + prSeq(xs)
    } else if x == .A2p 
                || x == .A1p 
                || x == .Tp 
                || x == .U1p 
                || x == .U2p
                || x == .Yp
                || x == .R2p
                || x == .Zp 
                || x == .R1p
                || x == .Hp 
                || x == .G2p
                || x == .J2p
                || x == .G4p
                || x == .J1p
                || x == .G1p
                || x == .G3p
                || x == .Fp
                || x == .Vp 
                || x == .N2p
                || x == .N1p
                || x == .Ep { return "\nOLL -----\n " + prSeq(xs)
    } else { return x.rawValue + " " + prSeq(xs)
    }
}



class Surface {
    var c1, c2, c3, c4, c5, c6, c7, c8: Color
    init(_ c: [Color]) {
        self.c1 = c[0]
        self.c2 = c[1]
        self.c3 = c[2]
        self.c4 = c[3]
        self.c5 = c[4]
        self.c6 = c[5]
        self.c7 = c[6]
        self.c8 = c[7]
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
        return Surface([c1, c2, c3, c4, c5, c6, c7, c8])
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
        return w.check(.White) && r.check(.Red) && b.check(.Blue) &&
          o.check(.Orange) && g.check(.Green) && y.check(.Yellow)
    }

    func turn(_ op:Op) -> Cube {
        let oldCube = dupCube()
        let (or, oy, oo, ow, ob, og) =
          (oldCube.r, oldCube.y, oldCube.o, oldCube.w, oldCube.b, oldCube.g)
        switch op {
        case .R:
            (w.c3, w.c4, w.c5) = (or.c3, or.c4, or.c5)
            (r.c3, r.c4, r.c5) = (oy.c3, oy.c4, oy.c5)
            (y.c3, y.c4, y.c5) = (oo.c7, oo.c8, oo.c1)
            (o.c1, o.c8, o.c7) = (ow.c5, ow.c4, ow.c3)
            (b.c1, b.c2, b.c3, b.c4) = (ob.c3, ob.c4, ob.c5, ob.c6)
            (b.c5, b.c6, b.c7, b.c8) = (ob.c7, ob.c8, ob.c1, ob.c2)
            return self
        case .Y:
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
        case .Z:
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
        case .R_: return self.turn(.R).turn(.R).turn(.R)
        case .R2: return self.turn(.R).turn(.R)
        case .Y_: return self.turn(.Y).turn(.Y).turn(.Y)
        case .Z2: return self.turn(.Z).turn(.Z)
        case .Z_: return self.turn(.Z).turn(.Z).turn(.Z)
        case .L: return self.applySeq([.Y, .Y, .R, .Y, .Y])
        case .L2: return self.turn(.L).turn(.L)
        case .L_: return self.applySeq([.Y, .Y, .R_, .Y, .Y])
        case .U: return self.turn(.Z).turn(.R).turn(.Z_)
        case .U2: return self.turn(.U).turn(.U)
        case .U_: return self.turn(.Z).turn(.R_).turn(.Z_)
        case .D: return self.turn(.Z_).turn(.R).turn(.Z)
        case .D2: return self.turn(.D).turn(.D)
        case .D_: return self.turn(.Z_).turn(.R_).turn(.Z)
        case .F: return self.turn(.Y_).turn(.R).turn(.Y)
        case .F2: return self.turn(.F).turn(.F)
        case .F_: return self.turn(.Y_).turn(.R_).turn(.Y)
        case .B: return self.turn(.Y).turn(.R).turn(.Y_)
        case .B2: return self.turn(.B).turn(.B)
        case .B_: return self.turn(.Y).turn(.R_).turn(.Y_)
        case .Y2: return self.turn(.Y).turn(.Y)
        case .Y_2: return self.turn(.Y_).turn(.Y_)
        case .Z_2: return self.turn(.Z_).turn(.Z_)
        case .U_2: return self.turn(.U_).turn(.U_)
        case .B_2: return self.turn(.B_).turn(.B_)
        case .D_2: return self.turn(.D_).turn(.D_)
        case .L_2: return self.turn(.L_).turn(.L_)
        case .R_2: return self.turn(.R_).turn(.R_)
        case .F_2: return self.turn(.F_).turn(.F_)
        case .M: return self.applySeq([.Z, .D_, .U, .Y_, .Z_])
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


let goal = Cube(w: Surface([.White, .White, .White, .White, 
                            .White, .White, .White, .White]),
                r: Surface([.Red, .Red, .Red, .Red, 
                            .Red, .Red, .Red, .Red]),
                b: Surface([.Blue, .Blue, .Blue, .Blue, 
                            .Blue, .Blue, .Blue, .Blue]),
                o: Surface([.Orange, .Orange, .Orange, .Orange, 
                            .Orange, .Orange, .Orange, .Orange]),
                g: Surface([.Green, .Green, .Green, .Green, 
                            .Green, .Green, .Green, .Green]),
                y: Surface([.Yellow, .Yellow, .Yellow, .Yellow, 
                            .Yellow, .Yellow, .Yellow, .Yellow]))


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

/*
var pats:[String] = []
var pat:String? = nil
var solved = false
print("Input a scrambled pattern:")
pat = readLine(strippingNewline:true)
if let pat2 = pat {
    let q = fromStringPos(pat2)
    print(q.pr())
    let outs = solveQ(q)
    print(prSeq(outs))
    repeat {
        pats = []
        print("Input a surface:")
        let ops = readSurface(q)
        if let ops2 = ops {
            _ = q.applySeq(ops2)
            print(q.pr())
            let outs = solveQ(q)
            print(prSeq(outs))
        } else {
            break
        }
    } while !solved
}
*/
