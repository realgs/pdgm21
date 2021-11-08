// Jakub Szwedowicz


// Task 1
// Non-Tail recursion

def find(args: List[String], params: List[String]): List[String] =
  def arg_contains_param(arg: String, param: String, contIt: Int): Boolean =
    if arg.length - contIt < param.length then false
    else if arg_contains_param_helper(arg, param, contIt, 0, 0) then true
    else arg_contains_param(arg, param, contIt + 1)

  def arg_contains_param_helper(arg: String, param: String, f_it: Int, s_it: Int, acc: Int): Boolean =
    if acc == param.length then true
    else if arg.charAt(f_it) == param.charAt(s_it) then arg_contains_param_helper(arg, param, f_it + 1, s_it + 1, acc + 1)
    else false

  def arg_contains_params(arg: String, params: List[String]): Boolean = params match
    case h :: t =>
      if h.length != 0 && arg_contains_param(arg, h, 0) then true
      else arg_contains_params(arg, t)
    case Nil => false

  def find_helper(args: List[String], params: List[String]): List[String] = args match
    case h :: t =>
      if arg_contains_params(h, params) then h :: find_helper(t, params)
      else find_helper(t, params)
    case Nil => Nil

  find_helper(args, params)

find(List("Ala", "Ma", "Kota"), List("la", "ot"))
find(List("index0169", "iindex0168202", "iindex0168211", "iindex0168210", "iindex0169222", "index0169224"), List("index0168"))


// Task 1
// Tail recursion
def find_tail(args: List[String], params: List[String]): List[String] =
  def arg_contains_param(arg: String, param: String, contIt: Int): Boolean =
    if arg.length - contIt < param.length then false
    else if arg_contains_param_helper(arg, param, contIt, 0, 0) then true
    else arg_contains_param(arg, param, contIt + 1)

  def arg_contains_param_helper(arg: String, param: String, f_it: Int, s_it: Int, acc: Int): Boolean =
    if acc == param.length then true
    else if arg.charAt(f_it) == param.charAt(s_it) then arg_contains_param_helper(arg, param, f_it + 1, s_it + 1, acc + 1)
    else false

  def arg_contains_params(arg: String, params: List[String]): Boolean = params match
    case h :: t =>
      if h.length != 0 && arg_contains_param(arg, h, 0) then true
      else arg_contains_params(arg, t)
    case Nil => false

  def find_helper_tail(args: List[String], params: List[String], acc: List[String]): List[String] = args match
    case h :: t =>
      if arg_contains_params(h, params) then find_helper_tail(t, params, h :: acc)
      else find_helper_tail(t, params, acc)
    case Nil => Nil

  find_helper_tail(args, params, Nil)

find_tail(List("Ala", "Ma", "Kota"), List("la", "ot"))
find_tail(List("index0169", "iindex0168202", "iindex0168211", "iindex0168210", "iindex0169222", "index0169224"), List("index0168"))


// Task 2
// Non-Tail recursion
def join_Lists[A](fst: List[A], snd: List[A], trd: List[A]): List[A] = (fst, snd, trd) match
  case (h :: t, _, _) => h :: joinLists(t, snd, trd)
  case (Nil, h :: t, _) => h :: joinLists(fst, t, trd)
  case (Nil, Nil, h :: t) => h :: joinLists(fst, snd, t)
  case _ => Nil

join_Lists(List(1, 2, 3), List(10, 11), List(110, 111, 112))

// Task 2
// Tail recursion
def join_Lists_tail[A](fst: List[A], snd: List[A], trd: List[A], acc: List[A]): List[A] =
  def reverse[A](l: List[A], acc: List[A]): List[A] = l match
    case h :: t => reverse(t, h :: acc)
    case Nil => acc

  (fst, snd, trd) match
    case (h :: t, _, _) => joinListsTail(t, snd, trd, h :: acc)
    case (Nil, h :: t, _) => joinListsTail(fst, t, trd, h :: acc)
    case (Nil, Nil, h :: t) => joinListsTail(fst, snd, t, h :: acc)
    case _ => reverse(acc, Nil)

join_Lists_tail(List(1, 2, 3), List(10, 11), List(110, 111, 112), Nil)


