typealias Filename = String

struct Test {
    let testProp : String
	let testProp2 : [String:String]!
    let testTuple : (s : String, [Int], UInt)
}

enum TestEnum<T> {
	case A(T)
}

