structure Translate : TRANSLATE =
struct
  type exp = unit
  type level = int
  type access = level * Frame.access
  val outermost = 0
  fun newLevel {parent, name, formals} = parent + 1
  fun formals level = [(0, 0)]
  fun allocLocal level = (fn x: bool => (0, 0))
end