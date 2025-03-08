# Seiichi Ariga <seiichi.ariga@gmail.com>
# RPN Calculator exercise from Programming Praxis 
# https://programmingpraxis.com/2009/02/19/rpn-calculator/

# 使い方:
# テキストファイルに逆ポーランド記法で計算(四則演算)を用意して、
# $ cat TEXT.txt | ./RpmCalculator

# !: 今のところ、入力にエラーはご遠慮ください

import strutils, sequtils

# スタックを表現するためのtype
type
  Stack = object
    data: seq[string]
    counter: int # counterは、次に読み書きするスタックの位置

type 
  CalcStack = object
    val: seq[float]
    counter: int


func readData(stream: File = stdin): seq[string] = 
  # 標準入力を１行読み込む(改行は削除)
  # それぞれのトークンがスペース区切りなので、splitしてstring配列にする
  let inputStr = readLine(stdin)
  result = inputStr.split()

func initStack(d: seq[string] = @[]):Stack =
  # 計算用のスタック
  result.data = d
  result.counter = 0

func initCalcStack(): CalcStack =  # TODO: Genericsで書き直す
  result.val = @[]
  result.counter = 0

# 記号に合わせて計算を行う
func calc( a, b: float, ope: string):float =
  # Stringの比較で塩山を決める 
  # TODO: デフォルトの挙動を決める 
  case ope
  of "+":
    return  b + a
  of "-":
    return b - a
  of "*":
    return b * a
  of "/":
    return b / a
  else:
    return 123

func processStack(stack: var Stack, calcStack: var CalcStack): float =
  for val in stack.data:

    # floatか演算記号(+-*/)かによって、計算用スタックに積むか、演算実行かを決める
    # 下のifはfloatのstringの場合。最大１つの小数点を持つ半角数字かどうか
    if stack.data[stack.counter].all(isDigit) or (stack.data[stack.counter].count('.') == 1 and stack.data[stack.counter].allIt(it.isDigit or it == '.')):
      calcStack.val.add(parseFloat(stack.data[stack.counter]))
      stack.counter += 1
      calcStack.counter += 1
      
    else: # TODO: 入力エラーのチェックを追加
      var ans: float = calc(calcStack.val.pop(), calcStack.val.pop(), stack.data[stack.counter])
      stack.counter += 1

      calcStack.counter -= 2 # 計算が実行されると、数値が２個消費される
      calcStack.val.add(ans) # ２個消費した後に、今の計算結果をpushする
      calcStack.counter += 1
    result = calcStack.val[0]

when isMainModule:
  var stack = initStack(readData())
  # 計算用のスタックを作る
  var calcStack = initCalcStack()
  let answer = processStack(stack, calcStack)
  echo answer
