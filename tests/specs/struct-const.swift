typealias Filename = String

struct Test {
    let testProp : String
	let testProp2 : [String:String]!
}

enum TestEnum<T> {
	case A(T)
}