

class MyClass {

  init(val: String, noop1: String, nooop2: Int) {
    self.propertyConstant = val
  }

  init() {
    self.propertyConstant = "Random Value"
  }


  func instanceMethod() -> Void {
  }

  func instanceMethodWith(Param1: String, Param2: Int) -> Void {
  }

  func instanceMethodWithNoReturnsArrow() {
    print("foobar")
  }

  func instanceMethodWithComplexArgs(Param1: (String) -> Void, Param2: [String: Int]) {
    print("complex args")
  }

  let propertyConstant: String
  var propertyComputed: String {
      return  "a" + "b"
  }

  var propertyWithTypeAndAssignment: String = "Hello"

  var propertyWithOnlyAssignment = "world"

  public private(set) var privateSetProperty : String {
     get {
        return "Foo"
     }
     set {
      // Do nothing
     }
  }

  let propertyOfTypeClosure : () -> Void = {
		print("Doing something random here")
	}

  class func staticMethod() -> Void {
  }

  class func staticMethodWith(Param1: String, Param2: Int) -> Void {
  }

  class func staticMethodWithNoReturnsArrow() {
    print("foobar")
  }

  class func staticMethodWithComplexArgs(Param1: (String) -> Void, Param2: [String: Int]) {
    print("complex args")
  }


  class InnerClass {
      func innerClassFunction(var1: String, var2: String) {

      }

  deinit {
    print("Class instance destructed")
  }
  }


struct Structure {

  let propertyInsideStructure: String

  func functionInsideStructure(functionParameter: (String, String) -> Int) -> Structure {
      return self;
  }

  static func staticFunctionInsideStructure() -> Void {
    print("static function does nothing")
  }
}


}


func independentFunction(var1: String, var2: String) -> String {
  return "foobar"
}
