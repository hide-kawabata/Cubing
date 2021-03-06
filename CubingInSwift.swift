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
    case F; case F_ = "F'"; case D; case D_ = "D'"
    case Y; case Y_ = "Y'"; case Z; case Z_ = "Z'"; case X; case X_ = "X'"
    case Y2; case Y_2 = "Y'2"; case Z2; case Z_2 = "Z'2"; case X2; case X_2 = "X'2"
    case N; case M; case M_ = "M'"
    case R2; case R_2 = "R'2"; case U2; case U_2 = "U'2"; case B2; case B_2 = "B'2"
    case L2; case L_2 = "L'2"; case F2; case F_2 = "F'2"; case D2; case D_2 = "D'2"
//
    case FstLayer; case SndLayer; case SndLayer2; case PLL1p 
    case PLL21p; case PLL22p; case PLL23p; case PLL24p; case PLL25p; case PLL26p
    case A2p; case A1p; case Tp; case U1p; case U2p; case Yp; case R2p
    case Zp; case R1p; case Hp; case G2p; case J2p; case G4p; case J1p 
    case G1p; case G3p; case Fp; case Vp; case N2p; case N1p; case Ep
//
    case Nil
}

enum CubingError : Error {
    case ParseError
    case AmbiguousInfo
}

// helper functions
func s2c(_ c: String) -> Color {
    return Color(rawValue: c)!
}

func effective_op(_ op: Op) -> Bool {
    switch op {
    case .R: return true
    case .R_: return true
    case .U: return true
    case .U_: return true
    case .B: return true
    case .B_: return true
    case .L: return true
    case .L_: return true
    case .F: return true
    case .F_: return true
    case .D: return true
    case .D_: return true
    case .M: return true
    case .R2: return true
    case .R_2: return true
    case .U2: return true
    case .U_2: return true
    case .B2: return true
    case .B_2: return true
    case .L2: return true
    case .L_2: return true
    case .F2: return true
    case .F_2: return true
    case .D2: return true
    case .D_2: return true
    default: return false
    }
}

// simple parser
func fromString(_ str: String) -> [Op] {
    func s2op(_ s: String) -> Op {
        return Op(rawValue: s)!
    }

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

        func rotc8(_ i: Int, _ colors: (Color, Color, Color, Color,
                                        Color, Color, Color, Color)) ->
          (Color, Color, Color, Color, Color, Color, Color, Color) {
            let (c1, c2, c3, c4, c5, c6, c7, c8) = colors
            switch i {
            case 0: return (c1, c2, c3, c4, c5, c6, c7, c8)
            case 1: return (c3, c4, c5, c6, c7, c8, c1, c2)
            case 2: return (c5, c6, c7, c8, c1, c2, c3, c4)
            case 3: return (c7, c8, c1, c2, c3, c4, c5, c6)
            default: return (c1, c2, c3, c4, c5, c6, c7, c8) // error
            }
        }

        func eq8(_ cs1: (Color, Color, Color, Color, Color, Color, Color, Color),
                 _ cs2: (Color, Color, Color, Color, Color, Color, Color, Color)) -> Bool {
            let (cs11, cs12, cs13, cs14, cs15, cs16, cs17, cs18) = cs1
            let (cs21, cs22, cs23, cs24, cs25, cs26, cs27, cs28) = cs2
            return cs11 == cs21 && cs12 == cs22 && cs13 == cs23 && cs14 == cs24
              && cs15 == cs25 && cs16 == cs26 && cs17 == cs27 && cs18 == cs28
        }

        let (r, b, o, g, w, y) = (q.r, q.b, q.o, q.g, q.w, q.y)
        let (r1, r2, r3, r4, r5, r6, r7, r8) = (r.c1, r.c2, r.c3, r.c4, r.c5, r.c6, r.c7, r.c8)
        let (b1, b2, b3, b4, b5, b6, b7, b8) = (b.c1, b.c2, b.c3, b.c4, b.c5, b.c6, b.c7, b.c8)
        let (o1, o2, o3, o4, o5, o6, o7, o8) = (o.c1, o.c2, o.c3, o.c4, o.c5, o.c6, o.c7, o.c8)
        let (g1, g2, g3, g4, g5, g6, g7, g8) = (g.c1, g.c2, g.c3, g.c4, g.c5, g.c6, g.c7, g.c8)
        let (w1, w2, w3, w4, w5, w6, w7, w8) = (w.c1, w.c2, w.c3, w.c4, w.c5, w.c6, w.c7, w.c8)
        let (y1, y2, y3, y4, y5, y6, y7, y8) = (y.c1, y.c2, y.c3, y.c4, y.c5, y.c6, y.c7, y.c8)
        
        let colors1 = rotc8(1, colors)
        let colors2 = rotc8(2, colors)
        let colors3 = rotc8(3, colors)

        if eq8(colors, (r1, r2, y3, y4, y5, r6, r7, r8))
             || eq8(colors1, (r1, r2, y3, y4, y5, r6, r7, r8))
             || eq8(colors2, (r1, r2, y3, y4, y5, r6, r7, r8))
             || eq8(colors3, (r1, r2, y3, y4, y5, r6, r7, r8))
             || eq8(colors, (w1, w2, r3, r4, r5, w6, w7, w8))
             || eq8(colors1, (w1, w2, r3, r4, r5, w6, w7, w8))
             || eq8(colors2, (w1, w2, r3, r4, r5, w6, w7, w8))
             || eq8(colors3, (w1, w2, r3, r4, r5, w6, w7, w8))
             || eq8(colors, (o5, o6, w3, w4, w5, o2, o3, o4))
             || eq8(colors1, (o5, o6, w3, w4, w5, o2, o3, o4))
             || eq8(colors2, (o5, o6, w3, w4, w5, o2, o3, o4))
             || eq8(colors3, (o5, o6, w3, w4, w5, o2, o3, o4))
             || eq8(colors, (y1, y2, o7, o8, o1, y6, y7, y8))
             || eq8(colors1, (y1, y2, o7, o8, o1, y6, y7, y8))
             || eq8(colors2, (y1, y2, o7, o8, o1, y6, y7, y8))
             || eq8(colors3, (y1, y2, o7, o8, o1, y6, y7, y8)) {
            return .R
        } else if eq8(colors, (r1, r2, w3, w4, w5, r6, r7, r8))
                    || eq8(colors1, (r1, r2, w3, w4, w5, r6, r7, r8))
                    || eq8(colors2, (r1, r2, w3, w4, w5, r6, r7, r8))
                    || eq8(colors3, (r1, r2, w3, w4, w5, r6, r7, r8))
                    || eq8(colors, (w1, w2, o7, o8, o1, w6, w7, w8))
                    || eq8(colors1, (w1, w2, o7, o8, o1, w6, w7, w8))
                    || eq8(colors2, (w1, w2, o7, o8, o1, w6, w7, w8))
                    || eq8(colors3, (w1, w2, o7, o8, o1, w6, w7, w8))
                    || eq8(colors, (o5, o6, y3, y4, y5, o2, o3, o4))
                    || eq8(colors1, (o5, o6, y3, y4, y5, o2, o3, o4))
                    || eq8(colors2, (o5, o6, y3, y4, y5, o2, o3, o4))
                    || eq8(colors3, (o5, o6, y3, y4, y5, o2, o3, o4))
                    || eq8(colors, (y1, y2, r3, r4, r5, y6, y7, y8))
                    || eq8(colors1, (y1, y2, r3, r4, r5, y6, y7, y8))
                    || eq8(colors2, (y1, y2, r3, r4, r5, y6, y7, y8))
                    || eq8(colors3, (y1, y2, r3, r4, r5, y6, y7, y8)) {
            return .R_
        } else if eq8(colors, (r1, r2, o7, o8, o1, r6, r7, r8))
                    || eq8(colors1, (r1, r2, o7, o8, o1, r6, r7, r8))
                    || eq8(colors2, (r1, r2, o7, o8, o1, r6, r7, r8))
                    || eq8(colors3, (r1, r2, o7, o8, o1, r6, r7, r8))
                    || eq8(colors, (w1, w2, y3, y4, y5, w6, w7, w8))
                    || eq8(colors1, (w1, w2, y3, y4, y5, w6, w7, w8))
                    || eq8(colors2, (w1, w2, y3, y4, y5, w6, w7, w8))
                    || eq8(colors3, (w1, w2, y3, y4, y5, w6, w7, w8))
                    || eq8(colors, (o5, o6, r3, r4, r5, o2, o3, o4))
                    || eq8(colors1, (o5, o6, r3, r4, r5, o2, o3, o4))
                    || eq8(colors2, (o5, o6, r3, r4, r5, o2, o3, o4))
                    || eq8(colors3, (o5, o6, r3, r4, r5, o2, o3, o4))
                    || eq8(colors, (y1, y2, w3, w4, w5, y6, y7, y8))
                    || eq8(colors1, (y1, y2, w3, w4, w5, y6, y7, y8))
                    || eq8(colors2, (y1, y2, w3, w4, w5, y6, y7, y8))
                    || eq8(colors3, (y1, y2, w3, w4, w5, y6, y7, y8)) {
            return .R2
        } else if eq8(colors, (b3, b4, b5, b6, b7, b8, b1, b2))
                    || eq8(colors1, (b3, b4, b5, b6, b7, b8, b1, b2))
                    || eq8(colors2, (b3, b4, b5, b6, b7, b8, b1, b2))
                    || eq8(colors3, (b3, b4, b5, b6, b7, b8, b1, b2))
                    || eq8(colors, (g1, g2, g3, g4, g5, g6, g7, g8))
                    || eq8(colors1, (g1, g2, g3, g4, g5, g6, g7, g8))
                    || eq8(colors2, (g1, g2, g3, g4, g5, g6, g7, g8))
                    || eq8(colors3, (g1, g2, g3, g4, g5, g6, g7, g8))
                    || eq8(colors, (b7, b8, b1, b2, b3, b4, b5, b6))
                    || eq8(colors1, (b7, b8, b1, b2, b3, b4, b5, b6))
                    || eq8(colors2, (b7, b8, b1, b2, b3, b4, b5, b6))
                    || eq8(colors3, (b7, b8, b1, b2, b3, b4, b5, b6)) {
            return .N
        } else {
            return .Nil
        }
    }

    // e.g. no move detected -> [.N, .Nil, .N, .Nil, .Nil, .Nil] : Ns and Nils : Ambiguous
    //      no match -> [.Nil, .Nil, .Nil, .Nil, .Nil, .Nil] : all Nils : Error
    //      match -> [.R, .Nil, .Nil, .Nil, .Nil, .Nil] : one non-Nil and Nils : OK
    //      ambiguous -> [.R, .Nil, .L_, .Nil, .Nil, .Nil] : two or more non-Nils : Ambiguous
    func oneSurface(_ str: String) -> [Op] {
        let cs = str.characters.map({s2c("\($0)")}) // length: 8
        let colors = (cs[0], cs[1], cs[2], cs[3], cs[4], cs[5], cs[6], cs[7])
        let op1 = checkSurface(q, colors) // R, R', R2
        let op2 = checkSurface(q.dupCube().turn(.Y), colors) // B, B', B2
        let op3 = checkSurface(q.dupCube().turn(.Y).turn(.Y), colors) // L, L', L2
        let op4 = checkSurface(q.dupCube().turn(.Y_), colors) // F, F', F2
        let op5 = checkSurface(q.dupCube().turn(.Z), colors) // U, U', U2
        let op6 = checkSurface(q.dupCube().turn(.Z_), colors) // D, D', D2
        return [op1, op2, op3, op4, op5, op6]
    }

    func checkAmbiguity(_ l: [Op]) throws -> Bool { // ambiguous=T
        let c = l.map({(x:Op) -> Int in if x == .Nil { return 0 } else { return 1 }})
          .reduce(0, {$0 + $1})
        if c > 1 {
            return true // ambiguous
        } else if c == 1 {
            return false // not ambiguous
        } else { // c == 0
            print("Incorrect info")
            throw CubingError.AmbiguousInfo
        }
    }

    func mergeOps(_ l1: [Op], _ l2: [Op], _ acc: [Op]) -> [Op] {
        if l1.count == 0 { return acc }
        var l1t = l1
        var l2t = l2
        let l1h = l1t.remove(at: 0)
        let l2h = l2t.remove(at: 0)
        if l1h == .Nil || l2h == .Nil {
            return mergeOps(l1t, l2t, acc + [.Nil]) 
        } else {
            return mergeOps(l1t, l2t, acc + [l2h])
        }
    }

    var pats:[Op] = [.N, .N, .N, .N, .N, .N] // initial dummy value
    var ambiguous = true
    var i = 1
    while ambiguous && i <= 3 {
        print("trial no." + String(i))
        i = i + 1
        let sur = readLine(strippingNewline:true)
        if let sur2 = sur {
            let ops = oneSurface(sur2)
//            print(ops)
            pats = mergeOps(pats, ops, [])
//            print(pats)
            do {
                ambiguous = try checkAmbiguity(pats)
            } catch { // CubingError.AmbuguousInfo
                break // quit solving
            }
        } else {
            continue
        }
    }

    if i >= 4 {
        print("Please input more info")
        return nil
    } else if ambiguous == true { // error
        return nil
    } else {
        if pats[0] != .Nil {
            return [pats[0]]
        } else if pats[1] != .Nil {
            return [.Y, pats[1], .Y_]
        } else if pats[2] != .Nil {
            return [.Y, .Y, pats[2], .Y_, .Y_]
        } else if pats[3] != .Nil {
            return [.Y_, pats[3], .Y]
        } else if pats[4] != .Nil {
            return [.Z, pats[4], .Z_]
        } else if pats[5] != .Nil {
            return [.Z_, pats[5], .Z]
        } else {
            return nil
        }
    }
}

func rotOp(_ mode: Op, _ op: Op) -> Op {
    switch mode {
    case .N: return op
    case .Y:
        switch op {
        case .U: return op
        case .L: return .F
        case .R: return .B
        case .F: return .R
        case .B: return .L
        case .D: return op
        case .U_: return op
        case .L_: return .F_
        case .R_: return .B_
        case .F_: return .R_
        case .B_: return .L_
        case .D_: return op
        default: return op // error
        }
    case .Y_: 
        switch op {
        case .U: return op
        case .L: return .B
        case .R: return .F
        case .F: return .L
        case .B: return .R
        case .D: return op
        case .U_: return op
        case .L_: return .B_
        case .R_: return .F_
        case .F_: return .L_
        case .B_: return .R_
        case .D_: return op
        default: return op // error
        }
    case .Z:
        switch op {
        case .U: return .L
        case .L: return .B
        case .R: return .U
        case .F: return op
        case .B: return op
        case .D: return .R
        case .U_: return .L_
        case .L_: return .B_
        case .R_: return .U_
        case .F_: return op
        case .B_: return op
        case .D_: return .R_
        default: return op // error
        }
    case .Z_:
        switch op {
        case .U: return .R
        case .L: return .U
        case .R: return .B
        case .F: return op
        case .B: return op
        case .D: return .L
        case .U_: return .R_
        case .L_: return .U_
        case .R_: return .B_
        case .F_: return op
        case .B_: return op
        case .D_: return .L_
        default: return op // error
        }
    default: return op // error
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

func revOps(_ ops_arg: [Op]) -> [Op] {
    func revOps_(_ acc_arg: [Op], _ ops_arg: [Op]) -> [Op] {
        var ops = ops_arg
        if ops.count == 0 { 
            return acc_arg
        } else {
            let op = ops.remove(at: 0)
            return revOps_([revOp(op)] + acc_arg, ops)
        }
    }
    return revOps_([], ops_arg)
}

func expandOp(_ xxs_arg: [Op]) -> [Op] {
    func expandOp_(_ acc_arg: [Op], _ xxs_arg: [Op]) -> [Op] {
        var xxs = xxs_arg
        if xxs.count == 0 { // length: 0
            return acc_arg
        }
        let x = xxs.remove(at: 0) // length: >=1
        let xs = xxs
        switch x {
        case .R2: return expandOp_(acc_arg + [.R, .R], xs)
        case .L2: return expandOp_(acc_arg + [.L, .L], xs)
        case .D2: return expandOp_(acc_arg + [.D, .D], xs)
        case .U2: return expandOp_(acc_arg + [.U, .U], xs)
        case .B2: return expandOp_(acc_arg + [.B, .B], xs)
        case .F2: return expandOp_(acc_arg + [.F, .F], xs)
        case .Y2: return expandOp_(acc_arg + [.Y, .Y], xs)
        case .Z2: return expandOp_(acc_arg + [.Z, .Z], xs)
        case .R_2: return expandOp_(acc_arg + [.R, .R], xs)
        case .L_2: return expandOp_(acc_arg + [.L, .L], xs)
        case .D_2: return expandOp_(acc_arg + [.D, .D], xs)
        case .U_2: return expandOp_(acc_arg + [.U, .U], xs)
        case .B_2: return expandOp_(acc_arg + [.B, .B], xs)
        case .F_2: return expandOp_(acc_arg + [.F, .F], xs)
        case .Y_2: return expandOp_(acc_arg + [.Y, .Y], xs)
        case .Z_2: return expandOp_(acc_arg + [.Z, .Z], xs)
        default: return expandOp_(acc_arg + [x], xs)
        }
    }
    return expandOp_([], xxs_arg)
}


func exchangeOp(_ xxs_arg: [Op]) -> [Op] {
    func exchangeOp_(_ acc_arg: [Op], _ xxs_arg: [Op]) -> [Op] {
        var xxs = xxs_arg
        if xxs.count == 0 { // length: 0
            return acc_arg
        }
        let x = xxs.remove(at: 0)
        if xxs.count == 0 { // length: 1
            return acc_arg + [x]
        }
        let y = xxs.remove(at: 0) // length: >=2
        let xs = xxs
        switch (x, y) {
            // Y
        case (.Y, .U): return exchangeOp_(acc_arg + [y], [x] + xs)
        case (.Y, .U_): return exchangeOp_(acc_arg + [y], [x] + xs)
        case (.Y_, .U): return exchangeOp_(acc_arg + [y], [x] + xs)
        case (.Y_, .U_): return exchangeOp_(acc_arg + [y], [x] + xs)
        case (.Y, .D): return exchangeOp_(acc_arg + [y], [x] + xs)
        case (.Y, .D_): return exchangeOp_(acc_arg + [y], [x] + xs)
        case (.Y_, .D): return exchangeOp_(acc_arg + [y], [x] + xs)
        case (.Y_, .D_): return exchangeOp_(acc_arg + [y], [x] + xs)
        case (.Y, .FstLayer): return exchangeOp_(acc_arg + [y], [x] + xs)
        case (.Y_, .FstLayer): return exchangeOp_(acc_arg + [y], [x] + xs)
        case (.Y, .SndLayer): return exchangeOp_(acc_arg + [y], [x] + xs)
        case (.Y_, .SndLayer): return exchangeOp_(acc_arg + [y], [x] + xs)
        case (.Y, .SndLayer2): return exchangeOp_(acc_arg + [y], [x] + xs)
        case (.Y_, .SndLayer2): return exchangeOp_(acc_arg + [y], [x] + xs)
                         // D
        case (.D, .U): return exchangeOp_(acc_arg + [y], [x] + xs)
        case (.D, .U_): return exchangeOp_(acc_arg + [y], [x] + xs)
        case (.D_, .U): return exchangeOp_(acc_arg + [y], [x] + xs)
        case (.D_, .U_): return exchangeOp_(acc_arg + [y], [x] + xs)
                         // B
        case (.B, .F): return exchangeOp_(acc_arg + [y], [x] + xs)
        case (.B, .F_): return exchangeOp_(acc_arg + [y], [x] + xs)
        case (.B_, .F): return exchangeOp_(acc_arg + [y], [x] + xs)
        case (.B_, .F_): return exchangeOp_(acc_arg + [y], [x] + xs)
                         // L
        case (.L, .R): return exchangeOp_(acc_arg + [y], [x] + xs)
        case (.L, .R_): return exchangeOp_(acc_arg + [y], [x] + xs)
        case (.L_, .R): return exchangeOp_(acc_arg + [y], [x] + xs)
        case (.L_, .R_): return exchangeOp_(acc_arg + [y], [x] + xs)
                         //
        default: return exchangeOp_(acc_arg + [x], [y] + xs)
        }
    }
    return exchangeOp_([], xxs_arg)
}


func reduceOp(_ xxs_arg: [Op]) -> [Op] {
    func reduceOp_(_ acc_arg: [Op], _ xxs_arg: [Op]) -> [Op] {
        var xxs = xxs_arg
        if xxs.count == 0 { // length: 0
            return acc_arg
        }
        let x = xxs.remove(at: 0)
        let xs = xxs
        if xxs.count == 0 { // length: 1
            return acc_arg + [x]
        }
        let y = xxs.remove(at: 0) // length: >=2
        let ys = xxs
        if x == revOp(y) {
            return reduceOp_(acc_arg, ys)
        }
        if xxs.count >= 2 { // length: >=4
            let z = xxs.remove(at: 0)
            let zs = xxs
            let w = xxs.remove(at: 0)
            let ws = xxs
            if x == y && y == z && z == w {
                return reduceOp_(acc_arg, ws)
            } else if x == y && y == z {
                return reduceOp_(acc_arg, [revOp(x)] + zs)
            } else {
                return reduceOp_(acc_arg + [x], xs)
            }
        } else if xxs.count == 1 { // length: 3
            let z = xxs.remove(at: 0)
            if x == y && y == z {
                return acc_arg + [revOp(x)]
            } else {
                return reduceOp_(acc_arg + [x], xs)
            }
        } else { // length: 2, x != y
            return acc_arg + xxs_arg
        }
    }
    return reduceOp_([], xxs_arg)
}


func mergeOp(_ xxs_arg: [Op]) -> [Op] {
    func mergeOp_(_ acc_arg: [Op], _ xxs_arg: [Op]) -> [Op] {
        var xxs = xxs_arg
        if xxs.count == 0 { // length: 0
            return acc_arg
        }
        let x = xxs.remove(at: 0)
        let xs = xxs
        if xxs.count == 0 { // length: 1
            return acc_arg + [x]
        }
        let y = xxs.remove(at: 0)
        let ys = xxs
        switch (x, y) {
        case (.U, .U): return mergeOp_(acc_arg + [.U2], ys)
        case (.B, .B): return mergeOp_(acc_arg + [.B2], ys)
        case (.F, .F): return mergeOp_(acc_arg + [.F2], ys)
        case (.D, .D): return mergeOp_(acc_arg + [.D2], ys)
        case (.L, .L): return mergeOp_(acc_arg + [.L2], ys)
        case (.R, .R): return mergeOp_(acc_arg + [.R2], ys)
        case (.Y, .Y): return mergeOp_(acc_arg + [.Y2], ys)
        case (.Z, .Z): return mergeOp_(acc_arg + [.Z2], ys)
        case (.U_, .U_): return mergeOp_(acc_arg + [.U2], ys)
        case (.B_, .B_): return mergeOp_(acc_arg + [.B2], ys)
        case (.F_, .F_): return mergeOp_(acc_arg + [.F2], ys)
        case (.D_, .D_): return mergeOp_(acc_arg + [.D2], ys)
        case (.L_, .L_): return mergeOp_(acc_arg + [.L2], ys)
        case (.R_, .R_): return mergeOp_(acc_arg + [.R2], ys)
        case (.Y_, .Y_): return mergeOp_(acc_arg + [.Y2], ys)
        case (.Z_, .Z_): return mergeOp_(acc_arg + [.Z2], ys)
        default: return mergeOp_(acc_arg + [x], xs)
        }
    }
    return mergeOp_([], xxs_arg)
}

func removeYZ(_ xxs_arg: [Op]) -> [Op] {
    func removeYZ_(_ acc_arg: [Op], _ mode: Op, _ xxs_arg: [Op]) -> [Op] {
        var xxs = xxs_arg
        if xxs.count == 0 { // length: 0
            if mode == .N {
                return acc_arg
            } else {
                return acc_arg + [mode]
            }
        }
        let x = xxs.remove(at: 0)
        let xs = xxs
        switch mode {
        case .N: 
            switch x {
            case .Y: return removeYZ_(acc_arg, .Y, xs)
            case .Y_: return removeYZ_(acc_arg, .Y_, xs)
            case .Z: return removeYZ_(acc_arg, .Z, xs)
            case .Z_: return removeYZ_(acc_arg, .Z_, xs)
            default: return removeYZ_(acc_arg + [x], .N, xs)
            }
        default: if mode == revOp(x) {
                     return removeYZ_(acc_arg, .N, xs)
                 } else { // mode != .N
                     switch x {
                     case .Y: return removeYZ_(acc_arg + [x], mode, xs)
                     case .Y_: return removeYZ_(acc_arg + [x], mode, xs)
                     case .Z: return removeYZ_(acc_arg + [x], mode, xs)
                     case .Z_: return removeYZ_(acc_arg + [x], mode, xs)
                     default: return removeYZ_(acc_arg + [rotOp(mode, x)], mode, xs)
                     }
                 }
        }
    }
    return removeYZ_([], .N, xxs_arg)
}

func optimizeOp(_ l: [Op]) -> [Op] {
    func iterOpt(_ f: ([Op]) -> [Op], _ l: [Op]) -> [Op] {
        let l2 = f(l)
        if l == l2 {
            return l2
        } else {
            return iterOpt(f, l2)
        }
    }
    var l = expandOp(l)
//    l = iterOpt(exchangeOp, l)
    l = iterOpt(removeYZ, l)
    l = iterOpt(reduceOp, l)
    l = mergeOp(l)
    return l
}


// solver
func solveQ(_ q: Cube) -> [Op] {
    var pair = (q, [Op.FstLayer])
    pair = step(pair, setRY_tryD)
    pair = step(pair, arrangeNeedles)
    pair = step(pair, setRY_destructive(.N))
    pair = step(pair, setRY(.N))
    pair = step(pair, {[.Y] + setRY(.Y)($0.dupCube().turn(.Y)) + [.Y_]})
    pair = step(pair, {[.Y2] + setRY(.Y2)($0.dupCube().turn(.Y2)) + [.Y2]})
    pair = step(pair, {[.Y_] + setRY(.Y_)($0.dupCube().turn(.Y_)) + [.Y]})
    pair = step(pair, {(_) in [.SndLayer]})
    pair = step(pair, setYGRGR(.N))
    pair = step(pair, {[.Y] + setYGRGR(.Y)($0.dupCube().turn(.Y)) + [.Y_]})
    pair = step(pair, {[.Y2] + setYGRGR(.Y2)($0.dupCube().turn(.Y2)) + [.Y2]})
    pair = step(pair, {[.Y_] + setYGRGR(.Y_)($0.dupCube().turn(.Y_)) + [.Y]})
    pair = step(pair, {(_) in [.SndLayer]})
    pair = step(pair, setYGR(.N))
    pair = step(pair, {[.Y] + setYGR(.Y)($0.dupCube().turn(.Y)) + [.Y_]})
    pair = step(pair, {[.Y2] + setYGR(.Y2)($0.dupCube().turn(.Y2)) + [.Y2]})
    pair = step(pair, {[.Y_] + setYGR(.Y_)($0.dupCube().turn(.Y_)) + [.Y]})
    pair = step(pair, {(_) in [.SndLayer2]})
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


func setRY_tryD(_ q: Cube) -> [Op] {
  func countRight(_ op: Op, _ q: Cube) -> Int {
    let sr = q.r.getColor
    let sy = q.y.getColor
    let sb = q.b.getColor
    let sg = q.g.getColor
    let so = q.o.getColor
    let yellow = rotc(op, .Yellow)
    let red = rotc(op, .Red)
    let green = rotc(op, .Green)
    let blue = rotc(op, .Blue)
    let orange = rotc(op, .Orange)
    var count = 0
    if (sr(2) == red && sy(6) == yellow) { count = count + 1 }
    if (sb(2) == blue && sy(4) == yellow) { count = count + 1 }
    if (so(2) == orange && sy(2) == yellow) { count = count + 1 }
    if (sg(2) == green && sy(8) == yellow) { count = count + 1 }
    return count
  }
  func setRY2(_ tc: Op, _ q: Cube) -> [Op] {
    if countRight(tc, q) > 0 { return [] }
    let sy = q.y.getColor
    let sb = q.b.getColor
    let sg = q.g.getColor
    let so = q.o.getColor
    let yellow = rotc(tc, .Yellow)
    let red = rotc(tc, .Red)
    if sb(2) == red && sy(4) == yellow { return [.D_]
    } else if so(2) == red && sy(2) == yellow { return [.D, .D]
    } else if sg(2) == red && sy(8) == yellow { return [.D]
    } else { return []
    }
  }
  let opsR = setRY2(.N, q)
  if opsR != [] { return opsR }
  let opsB = setRY2(.Y, q.dupCube().turn(.Y))
  if opsB != [] { return [.Y] + opsB + [.Y_] }
  let opsO = setRY2(.Y2, q.dupCube().turn(.Y2))
  if opsO != [] { return [.Y2] + opsO + [.Y2] }
  let opsG = setRY2(.Y_, q.dupCube().turn(.Y_))
  if opsG != [] { return [.Y_] + opsO + [.Y] }

  return []
}


func setRY_destructive(_ tc: Op) -> (_ q: Cube) -> [Op] {
    func countRight(_ op: Op, _ q: Cube) -> Int {
        let sr = q.r.getColor
        let sy = q.y.getColor
        let sb = q.b.getColor
//        let sw = q.w.getColor
        let sg = q.g.getColor
        let so = q.o.getColor
        let yellow = rotc(op, .Yellow)
        let red = rotc(op, .Red)
        let green = rotc(op, .Green)
        let blue = rotc(op, .Blue)
        let orange = rotc(op, .Orange)
//        let white = rotc(op, .White)
        var count = 0
        if (sr(2) == red && sy(6) == yellow) {
            count = count + 1
        }
        if (sb(2) == blue && sy(4) == yellow) {
            count = count + 1
        }
        if (so(2) == orange && sy(2) == yellow) {
            count = count + 1
        }
        if (sg(2) == green && sy(8) == yellow) {
            count = count + 1
        }
        return count
    }
  func setRY2(_ q: Cube) -> [Op] {
      if countRight(tc, q) > 0 { return [] }

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
    } else if sr(2) == yellow && sy(6) == red { return [.F_, .R_, .D_] // [.F_, .D, .R_, .D_]
    } else if sr(4) == yellow && sb(8) == red { return [.R_, .D_] // [.D, .R_, .D_]
    } else if sr(4) == red && sb(8) == yellow { return [.F]
    } else if sr(6) == red && sw(2) == yellow { return [.F, .F]
    } else if sr(6) == yellow && sw(2) == red { return [.U_, .R_, .F] // [.U_, .R_, .F, .R]
    } else if sr(8) == yellow && sg(4) == red { return [.L, .D] // [.D_, .L, .D]
    } else if sr(8) == red && sg(4) == yellow { return [.F_]
//
    } else if sb(2) == red && sy(4) == yellow { return [.D_] // [.R, .D, .R_, .D_]
    } else if sb(2) == yellow && sy(4) == red { return [.R, .F]
    } else if sb(4) == yellow && so(8) == red { return [.B, .U, .U, .F, .F] // [.B, .U, .U, .B_, .F, .F]
    } else if sb(4) == red && so(8) == yellow { return [.R, .D_] // [.R_, .U, .R, .F, .F]
    } else if sb(6) == red && sw(4) == yellow { return [.U, .F, .F]
    } else if sb(6) == yellow && sw(4) == red { return [.R_, .F] // [.R_, .F, .R]
//
    } else if so(2) == red && sy(2) == yellow { return [.D_, .D_] // [.B, .B, .U, .U, .F, .F]
    } else if so(2) == yellow && sy(2) == red { return [.B, .R, .D_] // [.B, .B, .U, .R_, .F, .R]
    } else if so(4) == yellow && sg(8) == red { return [.L_, .D] // [.L, .U_, .L_, .F, .F]
    } else if so(4) == red && sg(8) == yellow { return [.B, .D, .D] // [.B_, .U_, .B, .U_, .F, .F]
    } else if so(6) == red && sw(6) == yellow { return [.U, .U, .F, .F]
    } else if so(6) == yellow && sw(6) == red { return [.U_, .L, .F_] // [.U_, .L, .F_, .L_]
//
    } else if sg(2) == red && sy(8) == yellow { return [.D] // [.L_, .D_, .L, .D]
    } else if sg(2) == yellow && sy(8) == red { return [.L_, .F_]
    } else if sg(6) == red && sw(8) == yellow { return [.U_, .F, .F]
    } else if sg(6) == yellow && sw(8) == red { return [.L, .F_] // [.L, .F_, .L_]
    } else { return [.U] // error_setRY
    }
  }

  return setRY2
}

func setRY_destructive_limited(_ tc: Op) -> (_ q: Cube) -> [Op] {
  func setRY2(_ q: Cube) -> [Op] {
    let sr = q.r.getColor
    let sy = q.y.getColor
    let sb = q.b.getColor
    let sw = q.w.getColor
    let sg = q.g.getColor
//    let so = q.o.getColor
    let yellow = rotc(tc, .Yellow)
    let red = rotc(tc, .Red)

    if sr(2) == red && sy(6) == yellow { return []
    } else if sr(4) == red && sb(8) == yellow { return [.F]
    } else if sr(6) == red && sw(2) == yellow { return [.F, .F]
    } else if sr(8) == red && sg(4) == yellow { return [.F_]
    } else { return [] // error_setRY
    }
  }
  return setRY2
}


func arrangeNeedles(_ q: Cube) -> [Op] {
    func countRight(_ op: Op, _ q: Cube) -> Int {
        let sr = q.r.getColor
        let sy = q.y.getColor
        let sb = q.b.getColor
        let sg = q.g.getColor
        let so = q.o.getColor
        let yellow = rotc(op, .Yellow)
        let red = rotc(op, .Red)
        let green = rotc(op, .Green)
        let blue = rotc(op, .Blue)
        let orange = rotc(op, .Orange)
        var count = 0
        if (sr(2) == red && sy(6) == yellow) {
            count = count + 1
        }
        if (sb(2) == blue && sy(4) == yellow) {
            count = count + 1
        }
        if (so(2) == orange && sy(2) == yellow) {
            count = count + 1
        }
        if (sg(2) == green && sy(8) == yellow) {
            count = count + 1
        }
        return count
    }
    func countNeedles(_ op: Op, _ q: Cube) -> Int {
        let sr = q.r.getColor
        let sy = q.y.getColor
        let sb = q.b.getColor
        let sw = q.w.getColor
        let sg = q.g.getColor
        let so = q.o.getColor
        let yellow = rotc(op, .Yellow)
        let red = rotc(op, .Red)
        let green = rotc(op, .Green)
        let blue = rotc(op, .Blue)
        let orange = rotc(op, .Orange)
        var count = 0
        if (sr(2) == red && sy(6) == yellow
              || sr(4) == red && sb(8) == yellow
              || sr(6) == red && sw(2) == yellow
              || sr(8) == red && sg(4) == yellow) {
            count = count + 1
        }
        if (sb(2) == blue && sy(4) == yellow
              || sb(4) == blue && so(8) == yellow
              || sb(6) == blue && sw(4) == yellow
              || sb(8) == blue && sr(4) == yellow) {
            count = count + 1
        }
        if (so(2) == orange && sy(2) == yellow
              || so(4) == orange && sg(8) == yellow
              || so(6) == orange && sw(6) == yellow
              || so(8) == orange && sb(4) == yellow) {
            count = count + 1
        }
        if (sg(2) == green && sy(8) == yellow
              || sg(4) == green && sr(8) == yellow
              || sg(6) == green && sw(8) == yellow
              || sg(8) == green && so(4) == yellow) {
            count = count + 1
        }
        return count
    }

    func check1Face(_ op: Op, _ q: Cube) -> ([Op], Cube) {
        let count0 = countNeedles(op, q)
        let o1 = setRY_destructive_limited(op)(q)
        let q1 = q.dupCube().applySeq(o1)
        let count1 = countNeedles(op, q1)
        if count0 <= count1 { 
//            print("check1Face:OK \(o1)")
            return (o1, q1)
        } else {
//            print("check1Face:NG")
            return ([], q)
        }
    }

    func iterArrangeNeedles(_ ops: [Op], _ q: Cube) -> ([Op], Cube) {
//        print("countNeedles=\(countNeedles(.N, q))")
        if countNeedles(.N, q) - countRight(.N, q) == 0 { return (ops, q) }
        else {
            let (ops_, _) = check1Face(.N, q)
            let ops1 = ops + ops_
            let q0 = q.dupCube().applySeq(ops_)
//            print(q0.pr())
            let (ops1_, _) = check1Face(.Y, q0.dupCube().turn(.Y))
            let ops2 = ops1 + [.Y] + ops1_ + [.Y_]
            let q1 = q0.dupCube().applySeq([.Y] + ops1_ + [.Y_])
//            print(q1.pr())
            let (ops2_, _) = check1Face(.Y2, q1.dupCube().turn(.Y2))
            let ops3 = ops2 + [.Y2] + ops2_ + [.Y2]
            let q2 = q1.dupCube().applySeq([.Y2] + ops2_ + [.Y2])
//            print(q2.pr())
            let (ops3_, _) = check1Face(.Y_, q2.dupCube().turn(.Y_))
            let ops4 = ops3 + [.Y_] + ops3_ + [.Y]
            let q3 = q2.dupCube().applySeq([.Y_] + ops3_ + [.Y])
//            print(q3.pr())
            return iterArrangeNeedles(ops4, q3)
        }
    }
    
    let (ops, _) = iterArrangeNeedles([], q.dupCube())
//    print("(arrangeNeedles)")
//    print(ops)
    return ops
}


func setYGRGR(_ tc: Op) -> (_ q: Cube) -> [Op] {
  func checkYGRGR(_ tc:Op, _ q: Cube) -> Bool {
    let sr = q.r.getColor
    let sg = q.g.getColor
    let green = rotc(tc, .Green)
    let red = rotc(tc, .Red)
    if sr(8) == red && sr(1) == red
         && sg(4) == green && sg(3) == green {
        return true
    } else {
        return false
    }
  }
  func setYGRGR2(_ q: Cube) -> [Op] {
    var ops: [Op] = []
    if checkYGRGR(tc, q) { return ops }
    ops = [.F, .U_, .F_]
    if checkYGRGR(tc, q.dupCube().applySeq(ops)) { return ops }
    ops = [.U] + [.F, .U_, .F_]
    if checkYGRGR(tc, q.dupCube().applySeq(ops)) { return ops }
    ops = [.U, .U] + [.F, .U_, .F_]
    if checkYGRGR(tc, q.dupCube().applySeq(ops)) { return ops }
    ops = [.U_] + [.F, .U_, .F_]
    if checkYGRGR(tc, q.dupCube().applySeq(ops)) { return ops }
    ops = [.L_, .U, .L]
    if checkYGRGR(tc, q.dupCube().applySeq(ops)) { return ops }
    ops = [.U] + [.L_, .U, .L]
    if checkYGRGR(tc, q.dupCube().applySeq(ops)) { return ops }
    ops = [.U, .U] + [.L_, .U, .L]
    if checkYGRGR(tc, q.dupCube().applySeq(ops)) { return ops }
    ops = [.U_] + [.L_, .U, .L]
    if checkYGRGR(tc, q.dupCube().applySeq(ops)) { return ops }
    ops = [.L, .U, .L_] + [.U_] + [.F, .U_, .F_]
    if checkYGRGR(tc, q.dupCube().applySeq(ops)) { return ops }
    ops = [.B, .U, .B_] + [.U, .U] + [.F, .U_, .F_]
    if checkYGRGR(tc, q.dupCube().applySeq(ops)) { return ops }
    ops = [.R, .U, .R_] + [.U] + [.F, .U_, .F_]
    if checkYGRGR(tc, q.dupCube().applySeq(ops)) { return ops }
    ops = [.L, .U_, .L_] + [.L_, .U, .L]
    if checkYGRGR(tc, q.dupCube().applySeq(ops)) { return ops }
    ops = [.B, .U_, .B_] + [.U_] + [.L_, .U, .L]
    if checkYGRGR(tc, q.dupCube().applySeq(ops)) { return ops }
    ops = [.R, .U_, .R_] + [.U_, .U_] + [.L_, .U, .L]
    if checkYGRGR(tc, q.dupCube().applySeq(ops)) { return ops }
    return []
  }
  return setYGRGR2
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
//        return [.Hp] + [.R_, .R_, .L, .L, .D_, .R_, .R_, .L, .L, .U_, .U_, .R_, .R_, .L, .L, .D_, .R_, .R_, .L, .L]
        return [.Hp] + [.R, .U_, .R, .U, .R, .U, .R, .U_, .R_, .U_, .R_, .R_]
          + [.U] + [.R, .U_, .R, .U, .R, .U, .R, .U_, .R_, .U_, .R_, .R_]
          + [.U_] // H = U2+[U]+U2+[U']
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

//    if x == .FstLayer { return "First Layer -----\n " + prSeq(xs)
//    } else if x == .SndLayer { return "\nSecond Layer -----\n " + prSeq(xs)
//    } else if x == .SndLayer2 { return "\nSecond Layer (2) -----\n " + prSeq(xs)
    if x == .FstLayer { return "Cross at the Bottom -----\n " + prSeq(xs)
    } else if x == .SndLayer { return "\nFirst Layer -----\n " + prSeq(xs)
    } else if x == .SndLayer2 { return "\nSecond Layer -----\n " + prSeq(xs)
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

func countSteps(_ ops_arg: [Op]) -> Int {
    func countSteps_(_ ops_arg: [Op], _ acc: Int) -> Int {
        if ops_arg.count == 0 { return acc }
        var xs = ops_arg
        let x = xs.remove(at: 0)
        if x == .FstLayer { return countSteps_(xs, acc) }
        else if x == .SndLayer { return countSteps_(xs, acc) }
        else if x == .SndLayer2 { return countSteps_(xs, acc) }
        else if x == .PLL1p
                || x == .PLL21p
                || x == .PLL22p
                || x == .PLL23p 
                || x == .PLL24p
                || x == .PLL25p 
                || x == .PLL26p { return countSteps_(xs, acc) }
        else if x == .A2p 
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
                || x == .Ep { return countSteps_(xs, acc) }
        else { return countSteps_(xs, acc + 1) }
    }
    return countSteps_(ops_arg, 0)
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

    func eq(_ s: Surface) -> Bool {
        return s.c1 == c1 && s.c2 == c2 && s.c3 == c3 &&
          s.c4 == c4 && s.c5 == c5 && s.c6 == c6
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
        case .X: return self.turn(.Z).turn(.Y_).turn(.Z_)
        case .X2: return self.turn(.X).turn(.X)
        case .X_: return self.turn(.X).turn(.X).turn(.X)
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

    func eq(_ q: Cube) -> Bool {
        return w.eq(q.w) && r.eq(q.r) && b.eq(q.b) &&
          o.eq(q.o) && g.eq(q.g) && y.eq(q.y)
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
    print("number of steps = \(countSteps(outs))")
//    print(outs)
    print(prSeq(outs))
    print("Solved:")
    print(q_start.dupCube().applySeq(outs).pr())
}

func applyAndPrint(_ ops_arg: [Op], _ q: Cube) {
    var ops = ops_arg
    if ops.count == 0 {return}
    let op = ops.remove(at: 0)
    let _ = q.turn(op)
    print(op)
    print(q.pr())
    return applyAndPrint(ops, q)
}

func solve_check_pat(_ str: String) {
    let q_start = fromStringPos(str)
    let outs = solveQ(q_start)
    print("Scrambled:")
    print(q_start.pr())
    print("Solution:")
//    print(outs)
    if q_start.dupCube().applySeq(outs).check() {
        print("number of steps = \(countSteps(outs))")
        print(prSeq(outs))
        print("Solved:")
        print(q_start.dupCube().applySeq(outs).pr())
    } else {
        print("Could not solve... Illegal configuration ?")
//        print(q_start.dupCube().applySeq(outs).pr())
//        applyAndPrint(outs, q_start.dupCube())
//        print("----------------------")
//        applyAndPrint(revOps(outs), q_start.dupCube().applySeq(outs))
    }
}

func first_effective_op(_ ops_arg: [Op]) -> ([Op], [Op]) {
    var ops = ops_arg
    var pre_ops: [Op] = []
    var ret_ops: [Op] = []
    var remaining: [Op] = []
    while ops.count != 0 {
        let op = ops.remove(at: 0)
        if op == .Y || op == .Y_ || op == .Y2 ||
             op == .Z || op == .Z_ || op == .Z2 {
            pre_ops = pre_ops + [op]
        } else if effective_op(op) {
            ret_ops = pre_ops + [op] + revOps(pre_ops)
            break
        } else {
            remaining = remaining + [op]
        }
    }
//    print("ARG:" + prSeq(ops_arg))
//    print("[1]:" + prSeq(ret_ops))
//    print("[2]:" + prSeq(optimizeOp(remaining + pre_ops + ops)))
    return (ret_ops, optimizeOp(remaining + pre_ops + ops))
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
    var outs = solveQ(q)
    print(prSeq(outs))
    repeat {
        pats = []
        print("Input a surface:")
        let ops = readSurface(q)
        if let ops2 = ops {
            var q2 = q.dupCube()
            _ = q.applySeq(ops2) // actual state
            print(q.pr())
            if q.check() {
                print("Finished!")
                break
            }
            let (fst, rst): ([Op], [Op]) = first_effective_op(outs)
            _ = q2.applySeq(fst) // followed state
//            print(q2.pr())
            if q2.eq(q) { // followed
                print("Followed:")
                outs = rst // continuation
            } else {
                print("Not followed:")
                outs = solveQ(q) // new solution
            }
            print(prSeq(outs))
        } else {
            break
        }
    } while !solved
}
*/
