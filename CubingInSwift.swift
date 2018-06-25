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

// helper functions
func i2c(_ num: Int) -> String {
    var c: String
    switch num {
    case 1: c = "W"
    case 2: c = "R"
    case 3: c = "B"
    case 4: c = "O"
    case 5: c = "G"
    case 6: c = "Y"
    default: c = "_"
    }
    return c
}

func c2i(_ c: String) -> Int {
    var i: Int
    switch c {
    case "W": i = 1
    case "R": i = 2
    case "B": i = 3
    case "O": i = 4
    case "G": i = 5
    case "Y": i = 6
    default: i = -1
    }
    return i
}


// simple parser
func fromString(_ str: String) -> [String] {
    func iter(_ xxs_arg: [String], _ acc_arg: [String]) -> [String] {
        var xxs = xxs_arg
        var acc = acc_arg

        if xxs.count == 0 {
            return acc
        }

        let x = xxs.remove(at: 0) // |xxs| >= 1
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
                            acc.append(x + xs0 + xs1)
                            return iter(xs, acc)
                        } else {
                            acc.append(x + xs0)
                            return iter(xs, acc)
                        }
                    } else {
                        acc.append(x + xs0)
                        return iter(xs, acc)
                    }
                } else if xs0 == "2" {
                    xs.remove(at: 0)
                    acc.append(x + xs0)
                    return iter(xs, acc)
                } else {
                    acc.append(x)
                    return iter(xs, acc)
                }
            } else {
                acc.append(x)
                return iter(xs, acc)
            }
        } else {
            acc.append(x)
            acc.append("error_fromString")
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
    return Cube(w: Surface(c2i(sw[0]), c2i(sw[1]), c2i(sw[2]), c2i(sw[3]),
                           c2i(sw[4]), c2i(sw[5]), c2i(sw[6]), c2i(sw[7])),
                r: Surface(c2i(sr[0]), c2i(sr[1]), c2i(sr[2]), c2i(sr[3]),
                           c2i(sr[4]), c2i(sr[5]), c2i(sr[6]), c2i(sr[7])),
                b: Surface(c2i(sb[0]), c2i(sb[1]), c2i(sb[2]), c2i(sb[3]),
                           c2i(sb[4]), c2i(sb[5]), c2i(sb[6]), c2i(sb[7])),
                o: Surface(c2i(so[0]), c2i(so[1]), c2i(so[2]), c2i(so[3]),
                           c2i(so[4]), c2i(so[5]), c2i(so[6]), c2i(so[7])),
                g: Surface(c2i(sg[0]), c2i(sg[1]), c2i(sg[2]), c2i(sg[3]),
                           c2i(sg[4]), c2i(sg[5]), c2i(sg[6]), c2i(sg[7])),
                y: Surface(c2i(sy[0]), c2i(sy[1]), c2i(sy[2]), c2i(sy[3]),
                           c2i(sy[4]), c2i(sy[5]), c2i(sy[6]), c2i(sy[7])))
}


func checkRevOp(_ x: String, _ y: String) -> Bool {
    switch (x, y) {
    case ("U", "U'"): return true
    case ("L", "L'"): return true
    case ("R", "R'"): return true
    case ("B", "B'"): return true
    case ("D", "D'"): return true
    case ("F", "F'"): return true
    case ("Y", "Y'"): return true
    case ("Z", "Z'"): return true
    case ("U'", "U"): return true
    case ("L'", "L"): return true
    case ("R'", "R"): return true
    case ("B'", "B"): return true
    case ("D'", "D"): return true
    case ("F'", "F"): return true
    case ("Y'", "Y"): return true
    case ("Z'", "Z"): return true
    default: return false
    }
}

func revOp(_ x: String) -> String {
    switch x {
    case "U": return "U'"
    case "L": return "L'"
    case "R": return "R'"
    case "B": return "B'"
    case "D": return "D'"
    case "F": return "F'"
    case "Y": return "Y'"
    case "Z": return "Z'"
    case "U'": return "U"
    case "L'": return "L"
    case "R'": return "R"
    case "B'": return "B"
    case "D'": return "D"
    case "F'": return "F"
    case "Y'": return "Y"
    case "Z'": return "Z"
    default: return x
    }
}

func expandOp(_ xxs_arg: [String]) -> [String] {
    var xxs = xxs_arg
    if xxs.count == 0 { // length: 0
        return []
    }
    let x = xxs.remove(at: 0) // length: >=1
    let xs = xxs
    switch x {
    case "R2": return ["R", "R"] + expandOp(xs)
    case "L2": return ["L", "L"] + expandOp(xs)
    case "D2": return ["D", "D"] + expandOp(xs)
    case "U2": return ["U", "U"] + expandOp(xs)
    case "B2": return ["B", "B"] + expandOp(xs)
    case "F2": return ["F", "F"] + expandOp(xs)
    case "Y2": return ["Y", "Y"] + expandOp(xs)
    case "Z2": return ["Z", "Z"] + expandOp(xs)
    case "R'2": return ["R", "R"] + expandOp(xs)
    case "L'2": return ["L", "L"] + expandOp(xs)
    case "D'2": return ["D", "D"] + expandOp(xs)
    case "U'2": return ["U", "U"] + expandOp(xs)
    case "B'2": return ["B", "B"] + expandOp(xs)
    case "F'2": return ["F", "F"] + expandOp(xs)
    case "Y'2": return ["Y", "Y"] + expandOp(xs)
    case "Z'2": return ["Z", "Z"] + expandOp(xs)
    default: return [x] + expandOp(xs)
    }
}


func exchangeOp(_ xxs_arg: [String]) -> [String] {
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
    case ("Y", "U"): return [y] + exchangeOp([x] + xs)
    case ("Y", "U'"): return [y] + exchangeOp([x] + xs)
    case ("Y'", "U"): return [y] + exchangeOp([x] + xs)
    case ("Y'", "U'"): return [y] + exchangeOp([x] + xs)
    case ("Y", "D"): return [y] + exchangeOp([x] + xs)
    case ("Y", "D'"): return [y] + exchangeOp([x] + xs)
    case ("Y'", "D"): return [y] + exchangeOp([x] + xs)
    case ("Y'", "D'"): return [y] + exchangeOp([x] + xs)
                       // D
    case ("D", "U"): return [y] + exchangeOp([x] + xs)
    case ("D", "U'"): return [y] + exchangeOp([x] + xs)
    case ("D'", "U"): return [y] + exchangeOp([x] + xs)
    case ("D'", "U'"): return [y] + exchangeOp([x] + xs)
                       // B
    case ("B", "F"): return [y] + exchangeOp([x] + xs)
    case ("B", "F'"): return [y] + exchangeOp([x] + xs)
    case ("B'", "F"): return [y] + exchangeOp([x] + xs)
    case ("B'", "F'"): return [y] + exchangeOp([x] + xs)
                       // L
    case ("L", "R"): return [y] + exchangeOp([x] + xs)
    case ("L", "R'"): return [y] + exchangeOp([x] + xs)
    case ("L'", "R"): return [y] + exchangeOp([x] + xs)
    case ("L'", "R'"): return [y] + exchangeOp([x] + xs)
                       //
    default: return [x] + exchangeOp([y] + xs)
    }
}


func reduceOp(_ xxs_arg: [String]) -> [String] {
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
    if checkRevOp(x, y) {
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


func iterOpt(_ f: ([String]) -> [String], _ l: [String]) -> [String] {
    let l2 = f(l)
    if l == l2 {
        return l2
    } else {
        return iterOpt(f, l2)
    }
}

func mergeOp(_ xxs_arg: [String]) -> [String] {
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
    case ("U", "U"): return ["U2"] + mergeOp(ys)
    case ("B", "B"): return ["B2"] + mergeOp(ys)
    case ("F", "F"): return ["F2"] + mergeOp(ys)
    case ("D", "D"): return ["D2"] + mergeOp(ys)
    case ("L", "L"): return ["L2"] + mergeOp(ys)
    case ("R", "R"): return ["R2"] + mergeOp(ys)
    case ("Y", "Y"): return ["Y2"] + mergeOp(ys)
    case ("Z", "Z"): return ["Z2"] + mergeOp(ys)
    case ("U'", "U'"): return ["U2"] + mergeOp(ys)
    case ("B'", "B'"): return ["B2"] + mergeOp(ys)
    case ("F'", "F'"): return ["F2"] + mergeOp(ys)
    case ("D'", "D'"): return ["D2"] + mergeOp(ys)
    case ("L'", "L'"): return ["L2"] + mergeOp(ys)
    case ("R'", "R'"): return ["R2"] + mergeOp(ys)
    case ("Y'", "Y'"): return ["Y2"] + mergeOp(ys)
    case ("Z'", "Z'"): return ["Z2"] + mergeOp(ys)
    default: return [x] + mergeOp(xs)
    }
}

func optimizeOp(_ l: [String]) -> [String] {
    var l = expandOp(l)
    l = iterOpt(exchangeOp, l)
    l = iterOpt(reduceOp, l)
    l = mergeOp(l)
    return l
}


// solver
func solveQ(_ q: Cube) -> [String] {
    var pair = (q, ["FstLayer"])
    pair = step(pair, setRY("N"))
    pair = step(pair, {["Y"] + setRY("Y")($0.dupCube().turn("Y")) + ["Y'"]})
    pair = step(pair, {["Y2"] + setRY("Y2")($0.dupCube().turn("Y2")) + ["Y2"]})
    pair = step(pair, {["Y'"] + setRY("Y'")($0.dupCube().turn("Y'")) + ["Y"]})
    pair = step(pair, {(_) in ["SndLayer"]})
    pair = step(pair, setYGR("N"))
    pair = step(pair, {["Y"] + setYGR("Y")($0.dupCube().turn("Y")) + ["Y'"]})
    pair = step(pair, {["Y2"] + setYGR("Y2")($0.dupCube().turn("Y2")) + ["Y2"]})
    pair = step(pair, {["Y'"] + setYGR("Y'")($0.dupCube().turn("Y'")) + ["Y"]})
    pair = step(pair, setGR("N"))
    pair = step(pair, {["Y"] + setGR("Y")($0.dupCube().turn("Y")) + ["Y'"]})
    pair = step(pair, {["Y2"] + setGR("Y2")($0.dupCube().turn("Y2")) + ["Y2"]})
    pair = step(pair, {["Y'"] + setGR("Y'")($0.dupCube().turn("Y'")) + ["Y"]})
    pair = step(pair, oneToThree)
    pair = step(pair, threeToFive)
    pair = step(pair, fiveToNine)
    pair = step(pair, {["Y"] + fiveToNine($0.dupCube().turn("Y")) + ["Y'"]})
    pair = step(pair, {["Y2"] + fiveToNine($0.dupCube().turn("Y2")) + ["Y2"]})
    pair = step(pair, {["Y'"] + fiveToNine($0.dupCube().turn("Y'")) + ["Y"]})
    pair = step(pair, nineToFinish)
    pair = step(pair, {["Y"] + nineToFinish($0.dupCube().turn("Y")) + ["Y'"]})
    pair = step(pair, {["Y2"] + nineToFinish($0.dupCube().turn("Y2")) + ["Y2"]})
    pair = step(pair, {["Y'"] + nineToFinish($0.dupCube().turn("Y'")) + ["Y"]})
    pair = step(pair, finishQ)
    let (_, ops) = pair
    return optimizeOp(ops)
}

func step(_ pair: (Cube, [String]), _ slvr: (Cube) -> [String]) -> (Cube, [String]) {
    let (q, ops) = pair
    let ops2 = slvr(q)
    let q2 = q.dupCube().applySeq(ops2)
    return (q2, ops + ops2)
}

func finishQ(_ q: Cube) -> [String] {
    if q.check() { return []
    } else if q.dupCube().turn("U").check() { return ["U"]
    } else if q.dupCube().turn("U2").check() { return ["U2"]
    } else if q.dupCube().turn("U'").check() { return ["U'"]
    } else { return ["error_finishQ"]
    }
}


func rotc(_ op:String, _ c:String) -> String {
    switch op {
    case "N": return c
    case "Y":
        switch c {
        case "R": return "B"
        case "B": return "O"
        case "O": return "G"
        case "G": return "R"
        default: return c
        }
    case "Y2":
        return rotc("Y", (rotc("Y", c)))
    case "Y'":
        return rotc("Y", (rotc("Y2", c)))
    default:
        return c
    }
            
}


func setRY(_ tc: String) -> (_ q: Cube) -> [String] {
  func setRY2(_ q: Cube) -> [String] {
    let sr = q.r.getColor
    let sy = q.y.getColor
    let sb = q.b.getColor
    let sw = q.w.getColor
    let sg = q.g.getColor
    let so = q.o.getColor
    let yellow = rotc(tc, "Y")
    let red = rotc(tc, "R")

    if sr(2) == red && sy(6) == yellow { return []
//
    } else if sr(2) == yellow && sy(6) == red { return ["F'", "D", "R'", "D'"]
    } else if sr(4) == yellow && sb(8) == red { return ["D", "R'", "D'"]
    } else if sr(4) == red && sb(8) == yellow { return ["F"]
    } else if sr(6) == red && sw(2) == yellow { return ["F", "F"]
    } else if sr(6) == yellow && sw(2) == red { return ["U'", "R'", "F", "R"]
    } else if sr(8) == yellow && sg(4) == red { return ["D'", "L", "D"]
    } else if sr(8) == red && sg(4) == yellow { return ["F'"]
//
    } else if sb(2) == red && sy(4) == yellow { return ["R", "D", "R'", "D'"]
    } else if sb(2) == yellow && sy(4) == red { return ["R", "F"]
    } else if sb(4) == yellow && so(8) == red { return ["B", "U", "U", "B'", "F", "F"]
    } else if sb(4) == red && so(8) == yellow { return ["R'", "U", "R", "F", "F"]
    } else if sb(6) == red && sw(4) == yellow { return ["U", "F", "F"]
    } else if sb(6) == yellow && sw(4) == red { return ["R'", "F", "R"]
//
    } else if so(2) == red && sy(2) == yellow { return ["B", "B", "U", "U", "F", "F"]
    } else if so(2) == yellow && sy(2) == red { return ["B", "B", "U", "R'", "F", "R"]
    } else if so(4) == yellow && sg(8) == red { return ["L", "U'", "L'", "F", "F"]
    } else if so(4) == red && sg(8) == yellow { return ["B'", "U'", "B", "U'", "F", "F"]
    } else if so(6) == red && sw(6) == yellow { return ["U", "U", "F", "F"]
    } else if so(6) == yellow && sw(6) == red { return ["U'", "L", "F'", "L'"]
//
    } else if sg(2) == red && sy(8) == yellow { return ["L'", "D'", "L", "D"]
    } else if sg(2) == yellow && sy(8) == red { return ["L'", "F'"]
    } else if sg(6) == red && sw(8) == yellow { return ["U'", "F", "F"]
    } else if sg(6) == yellow && sw(8) == red { return ["L", "F'", "L'"]
    } else { return ["error_setRY"]
    }
  }
  return setRY2
}


func setYGR(_ tc: String) -> (_ q: Cube) -> [String] {
  func setYGR2(_ q: Cube) -> [String] {
    let sr = q.r.getColor
    let sb = q.b.getColor
    let sg = q.g.getColor
    let so = q.o.getColor
    let yellow = rotc(tc, "Y")
    let green = rotc(tc, "G")
    let red = rotc(tc, "R")

    if sr(1) == red && sg(3) == green { return []
//
    } else if sr(1) == yellow && sg(3) == red { return ["F", "U", "F'", "U'", "F", "U", "F'"]
    } else if sr(1) == green && sg(3) == yellow { return ["L'", "U'", "L", "U", "L'", "U'", "L"]
//
    } else if sr(3) == yellow && sb(1) == green { return ["F'", "U'", "F", "U", "U", "L'", "U'", "L"]
    } else if sr(3) == red && sb(1) == yellow { return ["R", "U", "R'", "F", "U", "F'"]
    } else if sr(3) == green && sb(1) == red { return ["R", "U", "R'", "L'", "U'", "L"]
//
    } else if sr(5) == green && sb(7) == yellow { return ["L'", "U", "L"]
    } else if sr(5) == yellow && sb(7) == red { return ["U", "L'", "U'", "L"]
    } else if sr(5) == red && sb(7) == green { return ["U", "L'", "U", "L", "U'", "U'", "L'", "U'", "L"]
//
    } else if sr(7) == red && sg(5) == yellow { return ["L'", "U'", "L"]
    } else if sr(7) == green && sg(5) == red { return ["F", "U", "U", "F'", "U'", "F", "U", "F'"]
    } else if sr(7) == yellow && sg(5) == green { return ["F", "U", "F'"]
//
    } else if sb(3) == green && so(1) == red { return ["R'", "U'", "U'", "R", "F", "U", "F'"]
    } else if sb(3) == yellow && so(1) == green { return ["R'", "U'", "R", "U'", "L'", "U'", "L"]
    } else if sb(3) == red && so(1) == yellow { return ["B", "U", "B'", "U", "F", "U", "F'"]
//
    } else if sb(5) == green && so(7) == yellow { return ["U", "U", "F", "U", "F'"]
    } else if sb(5) == red && so(7) == green { return ["U", "U", "L'", "U", "L", "U", "U", "L'", "U'", "L"]
    } else if sb(5) == yellow && so(7) == red { return ["U", "U", "L'", "U'", "L"]
//
    } else if so(3) == red && sg(1) == yellow { return ["L", "U", "L'", "U", "U", "F", "U", "F'"]
    } else if so(3) == yellow && sg(1) == green { return ["B'", "U'", "B", "L'", "U'", "L"]
    } else if so(3) == green && sg(1) == red { return ["L", "U'", "L'", "U'", "F", "U", "F'"]
//
    } else if so(5) == yellow && sg(7) == red { return ["F", "U'", "F'"]
    } else if so(5) == green && sg(7) == yellow { return ["U'", "F", "U", "F'"]
    } else if so(5) == red && sg(7) == green { return ["F", "U", "U", "F'", "L'", "U'", "L"]
    } else { return ["error_setYGR"]
    }
  }
  return setYGR2
}


func setGR(_ tc: String) -> (_ q: Cube) -> [String] {
  func setGR2(_ q: Cube) -> [String] {
    let sr = q.r.getColor
    let sg = q.g.getColor
    let sw = q.w.getColor
    let sb = q.b.getColor
    let so = q.o.getColor
    let green = rotc(tc, "G")
    let red = rotc(tc, "R")

    if sr(8) == red && sg(4) == green { return []
//
    } else if sr(8) == green && sg(4) == red {
        return ["F", "U'", "F'", "U'", "L'", "U", "L", "U'", "F", "U'", "F'", "U'", "L'", "U", "L"]
    } else if sr(6) == red && sw(2) == green { return ["U'", "L'", "U", "L", "U", "F", "U'", "F'"]
    } else if sr(6) == green && sw(2) == red { return ["U", "U", "F", "U'", "F'", "U'", "L'", "U", "L"]
    } else if sr(4) == green && sb(8) == red { 
        return ["R", "U'", "R'", "U'", "F'", "U", "F", "U", "L'", "U", "L", "U", "F", "U'", "F'"]
    } else if sr(4) == red && sb(8) == green {
        return ["R", "U'", "R'", "U'", "F'", "U", "F", "F", "U'", "F'", "U'", "L'", "U", "L"]
//
    } else if sb(4) == green && so(8) == red {
        return ["Y", "R", "U'", "R'", "U'", "F'", "U", "F", "Y'", "U", "U", "L'", "U", "L", "U", "F", "U'", "F'"]
    } else if sb(4) == red && so(8) == green {
        return ["Y", "R", "U'", "R'", "U'", "F'", "U", "F", "Y'", "U", "F", "U'", "F'", "U'", "L'", "U", "L"]
    } else if sb(6) == red && sw(4) == green { return ["L'", "U", "L", "U", "F", "U'", "F'"]
    } else if sb(6) == green && sw(4) == red { return ["U'", "F", "U'", "F'", "U'", "L'", "U", "L"]
//
    } else if so(6) == red && sw(6) == green { return ["U", "L'", "U", "L", "U", "F", "U'", "F'"]
    } else if so(6) == green && sw(6) == red { return ["F", "U'", "F'", "U'", "L'", "U", "L"]
    } else if so(4) == red && sg(8) == green {
        return ["Y", "Y", "R", "U'", "R'", "U'", "F'", "U", "F", "Y", "Y", "U", "U", "F", "U'", "F'", "U'", "L'", "U", "L"]
    } else if so(4) == green && sg(8) == red {
        return ["Y", "Y", "R", "U'", "R'", "U'", "F'", "U", "F", "Y", "Y", "U'", "L'", "U", "L", "U", "F", "U'", "F'"]
//
    } else if sg(6) == red && sw(8) == green { return ["U", "U", "L'", "U", "L", "U", "F", "U'", "F'"]
    } else if sg(6) == green && sw(8) == red { return ["U", "F", "U'", "F'", "U'", "L'", "U", "L"]
    } else { return ["error_setGR"]
    }
  }
  return setGR2
}

// PLL1
func oneToThree(_ q: Cube) -> [String] {
    let swq = q.w.getColor
    if swq(2) != "W" && swq(4) != "W" &&
         swq(6) != "W" && swq(8) != "W" { 
        return ["PLL1p"] + ["F", "R", "U", "R'", "U'", "F'"]
    } else { return []
    }
}

// PLL2
func threeToFive(_ q: Cube) -> [String] {
    let sq = q.w.getColor
    let (w2, w4, w6, w8) = (sq(2), sq(4), sq(6), sq(8))
    
    if w2 == w4 && w2 != w6 && w2 != w8 { return ["PLL21p"] + ["B", "U", "L", "U'", "L'", "B'"]
    } else if  w4 == w6 && w4 != w8 && w4 != w2 { return ["PLL22p"] + ["U", "B", "U", "L", "U'", "L'", "B'"]
    } else if  w6 == w8 && w6 != w2 && w6 != w4 { return ["PLL23p"] + ["U", "U", "B", "U", "L", "U'", "L'", "B'"]
    } else if  w8 == w2 && w8 != w4 && w8 != w6 { return ["PLL24p"] + ["U'", "B", "U", "L", "U'", "L'", "B'"]
    } else if  w4 == w8 && w4 != w2 && w4 != w6 { return ["PLL25p"] + ["F", "R", "U", "R'", "U'", "F'"]
    } else if  w2 == w6 && w2 != w4 && w2 != w8 { return ["PLL26p"] + ["U", "F", "R", "U", "R'", "U'", "F'"]
    } else { return []
    }
}

// PLL3
func fiveToNine(_ q: Cube) -> [String] {
    let (r5, r7) = (q.r.getColor(5), q.r.getColor(7))
    let (o5, o7) = (q.o.getColor(5), q.o.getColor(7))
    let (g5, g7) = (q.g.getColor(5), q.g.getColor(7))
    let (b5, b7) = (q.b.getColor(5), q.b.getColor(7))
    let wq = q.w.getColor
    let (w1, w3, w5, w7) = (wq(1), wq(3), wq(5), wq(7))

    if r5 == o7 && o7 == g5 && g5 == g7 {
        return ["R", "U", "U", "R'", "R'", "U'", "R", "R", "U'", "R'", "R'", "U", "U", "R"]
    } else if  w1 == w3 && w3 == o5 && o5 == o7 {
        return ["R", "R", "D'", "R", "U", "U", "R'", "D", "R", "U", "U", "R"]
    } else if  r5 == b5 && b5 == o5 && o5 == w1 { return ["R", "U", "R'", "U", "R", "U'", "U'", "R'"]
    } else if  r7 == b7 && b7 == g7 && g7 == w5 { return ["R", "U'", "U'", "R'", "U'", "R", "U'", "R'"]
    } else if  r7 == r5 && r5 == o7 && o7 == o5 {
        return ["R", "U'", "U'", "R'", "U'", "R", "U", "R'", "U'", "R", "U'", "R'"]
    } else if  r7 == b5 && b5 == w3 && w3 == w7 {
        return ["L", "F'", "F'", "R'", "R'", "D", "R", "D'", "R", "F'", "F'", "L'"]
    } else if  r7 == w3 && w3 == w5 && w5 == o5 { return ["L", "F", "R'", "F'", "L'", "F", "R", "F'"]
    } else { return []
    }
}


// OLL
func nineToFinish(_ q: Cube) -> [String] {
    let (r5, r6, r7) = (q.r.getColor(5), q.r.getColor(6), q.r.getColor(7))
    let (g5, g6, g7) = (q.g.getColor(5), q.g.getColor(6), q.g.getColor(7))
    let (o5, o6, o7) = (q.o.getColor(5), q.o.getColor(6), q.o.getColor(7))
    let (b5, b6, b7) = (q.b.getColor(5), q.b.getColor(6), q.b.getColor(7))

    if g6 == g7 && o5 == o6 && b5 == b7 && b7 == r6 { // A1
        return ["A1p"] + ["R", "R", "F", "F", "R'", "B'", "R", "F", "F", "R'", "B", "R'"]
    } else if  g6 == g7 && o5 == o6 && r5 == r7 && r7 == b6 { // A2
        return ["A2p"] + ["R", "B'", "R", "F", "F", "R'", "B", "R", "F", "F", "R'", "R'"]
    } else if  r6 == r7 && g5 == g7 && g5 != g6 && o5 == o6 { // T
        return ["Tp"] + ["R", "U", "R'", "U'", "R'", "F", "R", "R", "U'", "R'", "U'", "R", "U", "R'", "F'"]
    } else if  o5 == o6 && o5 == o7 && r5 != r6 && r5 == r7 { 
        if r5 == b6 {
            return ["U1p"] + ["R", "R", "U", "R", "U", "R'", "U'", "R'", "U'", "R'", "U", "R'"] // U1
        } else {
            return ["U2p"] + ["R", "U'", "R", "U", "R", "U", "R", "U'", "R'", "U'", "R'", "R'"] // U2
        }
    } else if  r6 == r7 && b5 == b6 && r7 == o5 && b6 != b7 { // Y
        return ["Yp"] + ["F", "R", "U'", "R'", "U'", "R", "U", "R'", "F'", "R", "U", "R'", "U'", "R'", "F", "R", "F'"]
    } else if  r5 == r7 && r5 == b6 && r6 == b7 && g5 == g6 { // R2
        return ["R2p"] + ["R'", "U'", "U'", "R", "U'", "U'", "R'", "F", "R", "U", "R'", "U'", "R'", "F'", "R", "R", "U'"]
    } else if  r5 == r7 && r5 == b6 && r6 == b7 && b5 == b7 { // Z
        return ["Zp"] + ["R'", "L", "F'", "R", "R", "L'", "L'", "B'", "R", "R", "L'", "L'", "F'", "R'", "L", "D", "D", "R", "R", "L'", "L'", "U"]
    } else if  r5 == r7 && r5 == g6 && r6 == g5 && g7 != g5 { // R1
        return ["R1p"] + ["L", "U", "U", "L'", "U", "U", "L", "F'", "L'", "U'", "L", "U", "L", "F", "L'", "L'", "U"]
    } else if  r5 == r7 && b5 == b7 && r5 == o6 && b5 == g6 { // H
        return ["Hp"] + ["M", "M", "U'", "M", "M", "U'", "U'", "M", "M", "U'", "M", "M"]
    } else if  r5 == r6 && g5 == g7 && g7 == b6 && b7 == g6 { // G1
        return ["G1p"] + ["R", "R", "D", "Y", "R'", "U", "R'", "U'", "R", "D'", "Y'", "R'", "R'", "F'", "U", "F"]
    } else if  r5 == r6 && r6 == o7 && g5 == g6 && g6 == g7 { // J2
        return ["J2p"] + ["R", "U", "R'", "F'", "R", "U", "R'", "U'", "R'", "F", "R", "R", "U'", "R'", "U'"]
    } else if  r7 == r6 && r6 == o5 && b5 == b7 && b7 == g6 { // G3
        return ["G3p"] + ["L'", "L'", "D'", "Y'", "L", "U'", "L", "U", "L'", "D", "Y", "L", "L", "F", "U'", "F'"]
    } else if  r7 == r6 && r6 == o5 && b5 == b6 && b6 == b7 { // J1
        return ["J1p"] + ["L'", "U'", "L", "F", "L'", "U'", "L", "U", "L", "F'", "L'", "L'", "U", "L", "U"]
    } else if  r5 == r6 && o5 == o7 && o7 == b6 && r7 != r6 { // G2
        return ["G2p"] + ["F'", "U'", "F", "R", "R", "D", "Y", "R'", "U", "R", "U'", "R", "D'", "Y'", "R", "R"]
    } else if  r7 == r6 && o5 == o7 && o5 == g6 && r5 == b6 { // G4
        return ["G4p"] + ["F", "U", "F'", "L'", "L'", "D'", "Y'", "L", "U'", "L'", "U", "L'", "D", "Y", "L'", "L'"]
    } else if  r7 == b5 && r5 == b6 && r6 == b7 && g6 == g5 { // F
        return ["Fp"] + ["R'", "U'", "F'", "R", "U", "R'", "U'", "R'", "F", "R", "R", "U'", "R'", "U'", "R", "U", "R'", "U", "R"]
    } else if  r6 == r7 && g5 == g6 && g5 == b7 && r7 == o5 { // V
        return ["Vp"] + ["R'", "U", "R'", "U'", "Y", "R'", "F'", "R", "R", "U'", "R'", "U", "R'", "F", "R", "F", "Y'"]
    } else if  r7 == r6 && b7 == b6 && o7 == o6 && o6 == r5 && b6 != b5 && g5 != g6 { // N2
        return ["N2p"] + ["R'", "U", "R", "U'", "R'", "F'", "U'", "F", "R", "U", "R'", "F", "R'", "F'", "R", "U'", "R"]
    } else if  r5 == r6 && b5 == b6 && o5 == o6 && o6 == r7 && b6 != b7 { // N1
        return ["N1p"] + ["L", "U'", "L'", "U", "L", "F", "U", "F'", "L'", "U'", "L", "F'", "L", "F", "L'", "U", "L'"]
    } else if  r5 == b6 && b6 == o7 && r7 == g6 && g6 == o5 { // E
        return ["Ep"] + ["R", "B'", "R'", "F", "R", "B", "R'", "F'", "R", "B", "R'", "F", "R", "B'", "R'", "F'"]
    } else { return []
    }
}


func prSeq(_ ops_arg: [String]) -> String {
    if ops_arg.count == 0 { return "" }

    var xs = ops_arg
    let x = xs.remove(at: 0)

    if x == "FstLayer" { return "First Layer -----\n " + prSeq(xs)
    } else if x == "SndLayer" { return "\nSecond Layer -----\n " + prSeq(xs)
    } else if x == "PLL1p"
                || x == "PLL21p"
                || x == "PLL22p"
                || x == "PLL23p" 
                || x == "PLL24p"
                || x == "PLL25p" 
                || x == "PLL26p" { return "\nPLL -----\n " + prSeq(xs)
    } else if x == "A2p" 
                || x == "A1p" 
                || x == "Tp" 
                || x == "U1p" 
                || x == "U2p"
                || x == "Yp"
                || x == "R2p"
                || x == "Zp" 
                || x == "R1p"
                || x == "Hp" 
                || x == "G2p"
                || x == "J2p"
                || x == "G4p"
                || x == "J1p"
                || x == "G1p"
                || x == "G3p"
                || x == "Fp"
                || x == "Vp" 
                || x == "N2p"
                || x == "N1p"
                || x == "Ep" { return "\nOLL -----\n " + prSeq(xs)
    } else { return x + " " + prSeq(xs)
    }
}



class Surface {
    var c1, c2, c3, c4, c5, c6, c7, c8: Int
    
    init(_ c1: Int, _ c2: Int, _ c3: Int, _ c4: Int,
         _ c5: Int, _ c6: Int, _ c7: Int, _ c8: Int) {
        self.c1 = c1
        self.c2 = c2
        self.c3 = c3
        self.c4 = c4
        self.c5 = c5
        self.c6 = c6
        self.c7 = c7
        self.c8 = c8
    }
    
    func getColor(_ num: Int) -> String {
        var i: Int
        switch num {
        case 1: i = c1
        case 2: i = c2
        case 3: i = c3
        case 4: i = c4
        case 5: i = c5
        case 6: i = c6
        case 7: i = c7
        case 8: i = c8
        default : i = -1
        }
        return i2c(i)
    }
    
    func check(_ c: String) -> Bool {
        return getColor(1) == c && getColor(2) == c && getColor(3) == c &&
          getColor(4) == c && getColor(5) == c && getColor(6) == c
    }

    func dupSurface() -> Surface {
        return Surface(c1, c2, c3, c4, c5, c6, c7, c8)
    }
}

class Cube {
    var w: Surface
    var r: Surface
    var b: Surface
    var o: Surface
    var g: Surface
    var y: Surface
    
    init(w: Surface, r: Surface, b: Surface, o: Surface, g: Surface, y: Surface) {
        self.w = w
        self.r = r
        self.b = b
        self.o = o
        self.g = g
        self.y = y
    }
    
    func pr() -> String {
        let fw = self.w.getColor
        let fr = self.r.getColor
        let fb = self.b.getColor
        let fo = self.o.getColor
        let fg = self.g.getColor
        let fy = self.y.getColor
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
        return w.check("W") && r.check("R") && b.check("B") &&
          o.check("O") && g.check("G") && y.check("Y")
    }

    func turn(_ op:String) -> Cube {
        let oldCube = dupCube()
        let (or, oy, oo, ow, ob, og) =
          (oldCube.r, oldCube.y, oldCube.o, oldCube.w, oldCube.b, oldCube.g)
        switch op {
        case "R":
            (w.c3, w.c4, w.c5) = (or.c3, or.c4, or.c5)
            (r.c3, r.c4, r.c5) = (oy.c3, oy.c4, oy.c5)
            (y.c3, y.c4, y.c5) = (oo.c7, oo.c8, oo.c1)
            (o.c1, o.c8, o.c7) = (ow.c5, ow.c4, ow.c3)
            (b.c1, b.c2, b.c3, b.c4) = (ob.c3, ob.c4, ob.c5, ob.c6)
            (b.c5, b.c6, b.c7, b.c8) = (ob.c7, ob.c8, ob.c1, ob.c2)
            return self
        case "Y":
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
        case "Z":
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
        case "R'":
            return self.turn("R").turn("R").turn("R")
        case "R2":
            return self.turn("R").turn("R")
        case "Y'":
            return self.turn("Y").turn("Y").turn("Y")
        case "Z2":
            return self.turn("Z").turn("Z")
        case "Z'":
            return self.turn("Z").turn("Z").turn("Z")
        case "L":
            return self.applySeq(["Y", "Y", "R", "Y", "Y"])
        case "L2":
            return self.turn("L").turn("L")
        case "L'":
            return self.applySeq(["Y", "Y", "R'", "Y", "Y"])
        case "U":
            return self.turn("Z").turn("R").turn("Z'")
        case "U2":
            return self.turn("U").turn("U")
        case "U'":
            return self.turn("Z").turn("R'").turn("Z'")
        case "D":
            return self.turn("Z'").turn("R").turn("Z")
        case "D2":
            return self.turn("D").turn("D")
        case "D'":
            return self.turn("Z'").turn("R'").turn("Z")
        case "F":
            return self.turn("Y'").turn("R").turn("Y")
        case "F2":
            return self.turn("F").turn("F")
        case "F'":
            return self.turn("Y'").turn("R'").turn("Y")
        case "B":
            return self.turn("Y").turn("R").turn("Y'")
        case "B2":
            return self.turn("B").turn("B")
        case "B'":
            return self.turn("Y").turn("R'").turn("Y'")
        case "Y2":
            return self.turn("Y").turn("Y")
        case "Y'2":
            return self.turn("Y'").turn("Y'")
        case "Z'2":
            return self.turn("Z'").turn("Z'")
        case "U'2":
            return self.turn("U'").turn("U'")
        case "B'2":
            return self.turn("B'").turn("B'")
        case "D'2":
            return self.turn("D'").turn("D'")
        case "L'2":
            return self.turn("L'").turn("L'")
        case "R'2":
            return self.turn("R'").turn("R'")
        case "F'2":
            return self.turn("F'").turn("F'")
        case "M":
            return self.applySeq(["Z", "D'", "U", "Y'", "Z'"])
        default: break
//            return self.turn("R").turn("R'")
        }
        return self
    }

    func turn2(_ q:Cube, _ op:String) -> Cube {
        return q.turn(op)
    }

    func applySeq(_ l:[String]) -> Cube {
        return l.reduce(self, turn2)
    }
}


let goal = Cube(w: Surface(c2i("W"),c2i("W"),c2i("W"),c2i("W"),
                           c2i("W"),c2i("W"),c2i("W"),c2i("W")),
                r: Surface(c2i("R"),c2i("R"),c2i("R"),c2i("R"),
                           c2i("R"),c2i("R"),c2i("R"),c2i("R")),
                b: Surface(c2i("B"),c2i("B"),c2i("B"),c2i("B"),
                           c2i("B"),c2i("B"),c2i("B"),c2i("B")),
                o: Surface(c2i("O"),c2i("O"),c2i("O"),c2i("O"),
                           c2i("O"),c2i("O"),c2i("O"),c2i("O")),
                g: Surface(c2i("G"),c2i("G"),c2i("G"),c2i("G"),
                           c2i("G"),c2i("G"),c2i("G"),c2i("G")),
                y: Surface(c2i("Y"),c2i("Y"),c2i("Y"),c2i("Y"),
                           c2i("Y"),c2i("Y"),c2i("Y"),c2i("Y")))


func solve(_ str: String) -> [String] {
    return solveQ(goal.dupCube().applySeq(fromString(str)))
}

func solve_check(_ str: String) {
    let ins = fromString(str)
    let outs = solve(str)
    let q_start = goal.dupCube().applySeq(ins)
    print("Scramble:")
    print(ins)
    print("Scrambled:")
    print(q_start.pr())
    print("Solution:")
    print(outs)
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
    print(outs)
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
