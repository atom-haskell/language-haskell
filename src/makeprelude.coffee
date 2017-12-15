#!/bin/bash

CP = require 'child_process'
fs = require 'fs'

genp = CP.execSync("ghc-mod browse -dpo Prelude", encoding: 'utf8').split(/\r?\n|\r/).slice(0, -1)

arr = (input) ->
  lines =
    input.map (line) ->
      line.split(/ :: /)[0] or line
  return lines

run = (filter) ->
  if typeof filter isnt "function"
    rx = filter
    filter = (line) -> line.match(rx)
  p = arr(genp.filter filter)
  return p

exp = {}

exp.classes = run /class/
exp.funct = run /^[a-z]/
exp.constr = run /^[A-Z].*from:/
exp.types = run /data|type/
exp.operators = run(/^\(/).map((x) -> x.slice(1, -1))

exp.types.push (run (line) ->
  line.match(/^[A-Z]/) and not line.match(/::/) and not line.match(/True|False/))...
exp.constr.push 'True', 'False'

output = [ '# coffeelint: disable' ]
for k, v of exp
  output.push "#{k} = #{JSON.stringify(v)}"
output.push "module.exports = { #{Object.keys(exp)} }"

fs.writeFileSync 'src/include/prelude.coffee', output.join('\n')+'\n', 'utf8'
