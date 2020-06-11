from functools import reduce, partial

from graphviz import Digraph

from .dependencies import Var

def draw_atom_dot(dot, full_path, atom):
    (cl_id, lit_id) = full_path[0]
    rank = (lit_id > 0) * 2 * len(full_path)
    dot.node(f"rank{rank-1}", style='invis', shape='point')
    with dot.subgraph() as sub:
        sub.attr(rank='same')
        sub.node(f"rank{rank}", style='invis', shape='point')

        color_mapping = {None: None, True: 'green', False: 'red'}
        sub.node(str(full_path), 
                 f"{atom.predicate}({atom.code_args})", 
                 color=color_mapping[atom.success])
        if lit_id > 1:
            full_path_prev_atom = ((full_path[0][0],full_path[0][1] -1),) + full_path[1:]
            dot.edge(str(full_path_prev_atom), str(full_path), style='invis')
        if rank > 1:
            dot.edge(f"rank{rank-1}", f"rank{rank}", style='invis')

def execution_forest_to_dot(ef, filename='execution_forest', view=True):
    nodes, outgoing, _ = ef
    dot = Digraph()

    for full_path, atom in nodes.items():
        lit_id = full_path[0][1]
        if lit_id == 0 and len(full_path) > 1:
            continue # skip extraneous head nodes, i.e. those for recursive calls
        draw_atom_dot(dot, full_path, atom)

    for origin, dests in outgoing.items():
        for dest in dests:
            lit_id_origin = origin[0][1]
            if (lit_id_origin == 0 and len(origin) == 1) or len(origin) < len(dest):
                dot.edge(str(origin), str(dest), style='dashed')
            else:
                dot.edge(str(origin), str(dest))

    dot.render(filename, view=view)
    return dot

def draw_var_dot(dot, full_path_var, names):
    rank = 2 * len(full_path_var[0]) + 1
    with dot.subgraph() as sub:
        sub.attr(rank='same')
        sub.node(f"rank{rank}", style='invis', shape='point')

        display_names = ','.join(map(lambda n: chr(ord('A') + n), names))
        sub.node(f"{full_path_var}", display_names, shape='square')
        if rank > 1:
            dot.edge(f"rank{rank-1}", f"rank{rank}", style='invis')

def dependency_forest_to_dot(df, filename='dependency_forest', view=True):
    nodes, outgoing, _ = df
    dot = Digraph()

    for pos_ident, atom in nodes.items():
        if isinstance(pos_ident, Var):
            draw_var_dot(dot, ((), pos_ident), pos_ident.names)
        else:
            draw_atom_dot(dot, (pos_ident,), atom)

    def node_to_str(node):
        if isinstance(node, Var):
            return f"((), {node})"
        return str((node,))

    for origin, dests in outgoing.items():
        for dest in dests:
            dot.edge(node_to_str(origin), node_to_str(dest))

    dot.render(filename, view=view)
    return dot


def dependency_execution_forest_to_dot(def_, filename='dependency_execution_forest', view=True):
    nodes, outgoing, _ = def_
    dot = Digraph()

    is_var_node = lambda n: len(n) == 2 and isinstance(n[1], Var)

    for full_path_var, atom in nodes.items():
        if is_var_node(full_path_var):
            draw_var_dot(dot, full_path_var, full_path_var[1].names)
        else:
            lit_id = full_path_var[0][1]
            if lit_id == 0 and len(full_path_var) > 1:
                continue # skip extraneous head nodes, i.e. those for recursive calls
            draw_atom_dot(dot, full_path_var, atom)

    def node_to_str(node):
        if isinstance(node, Var):
            return f"Var(({node.clause},), {node.names})"
        return str((node,))

    for origin, dests in outgoing.items():
        for dest in dests:
            dot.edge(str(origin), str(dest))

    dot.render(filename, view=view)
    return dot
