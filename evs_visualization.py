# coding: utf-8
get_ipython().run_line_magic('load_ext', 'autoreload')
get_ipython().run_line_magic('autoreload', '2')
import popper.generate
Generate = popper.generate.Generate("/home/rolf/popper/examples/evs/modes.pl", debug=True) # not really necessary, just convenient for obtaining modeh
import popper.test
Test = popper.test.AnalyseProlog(Generate.modeh, bk_file="/home/rolf/popper/examples/evs/bk.pl", debug=True)
from popper.representation import from_code
from popper.representation.analyse import render, dependencies, execution_forest
EVS_MODES = """\
modeh(evs,1). 
direction(evs,0,in). 
modeb(empty,1). 
direction(empty,0,in). 
modeb(head,2). 
direction(head,0,in). 
direction(head,1,out). 
modeb(even,1). 
direction(even,0,in). 
modeb(odd,1). 
direction(odd,0,in). 
modeb(tail,2). 
direction(tail,0,in). 
direction(tail,1,out). 
modeb(evs,1). 
direction(evs,0,in). 
"""
EVS_CODE = """\
evs(A) :- empty(A).
evs(A) :- head(A,B),even(B),tail(A,C),head(C,D),odd(D),tail(C,E),evs(E).
"""
prog = from_code.from_strs(EVS_MODES, EVS_CODE)
Test.assert_ordered_program(prog)
res, subprogs = Test.evaluate(prog, 'evs([0,1,2,3,4,5])')
assert res
if False:
    df = dependencies.program_to_dependency_forest(prog)
    def_ = dependencies.to_dep_exe_forest(df, ef)
    render.dependency_forest_to_dot(df, filename='dependency_forest')
    render.execution_forest_to_dot(ef, filename='execution_forest_success')
    render.dependency_execution_forest_to_dot(def_, filename='dependency_execution_forest_success')

res2, subprogs2 = Test.evaluate(prog, 'evs([0,1,2,3,4,6])')
assert not res2
if False:
    def_ = dependencies.to_dep_exe_forest(df, ef)
    render.execution_forest_to_dot(ef, filename='execution_forest_failure')
    render.dependency_execution_forest_to_dot(def_, filename='dependency_execution_forest_failure')
