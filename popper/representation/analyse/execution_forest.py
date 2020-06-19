def extract_succeeding_sub_programs(program, ef):
    nodes, _, _ = ef
    for path in filter(lambda path: nodes[path].success, extract_leafs(ef)):
        trace = set(trace_to_node(path, ef))
        yield trace_to_sub_program(program, trace)


def extract_failing_sub_programs(program, ef):
    nodes, _, _ = ef
    for path in filter(lambda path: not nodes[path].success, extract_leafs(ef)):
        trace = set(trace_to_node(path, ef))
        success_leafs = filter(lambda leaf_path: nodes[leaf_path].success,
                               leafs_in_trace_restricted_exe_forest(ef, trace))
        if any(True for _ in success_leafs):
            continue # sub-program proves the root

        yield trace_to_sub_program(program, trace)


def trace_to_sub_program(program, trace):
    sub_program = []
    for cl_id, head, body in program:
        body_atoms = []
        for body_atom_idx, body_atom in enumerate(body, 1):
            if (cl_id, body_atom_idx) in trace:
                body_atoms.append(body_atom)
            else:
                break
        if body_atoms != []:
           sub_program.append((cl_id, head, tuple(body_atoms)))
    return sub_program


def leafs_in_trace_restricted_exe_forest(ef, trace):
    nodes, outgoing, _ = ef
    roots, cl_id = [], 0
    while ((cl_id, 0),) in nodes.keys():
        roots.append(((cl_id, 0),))
        cl_id += 1
    stack = roots
    while stack != []:
        current = stack.pop()
        seen_children = 0
        for path in outgoing[current]:
            if path[0] in trace:
                seen_children += 1
                stack.append(path)
        if seen_children == 0:
            yield current


def trace_to_node(path, ef):
    nodes, _, incoming = ef
    parents = set((path,))
    while len(parents) != 0: # whilst not seen the root
        assert len(parents) == 1 # can only use this function for trees (not DAGs)
        for parent in parents: break # fastest way to obtain singleton contained
        yield parent[0]
        parents = incoming[parent]


def extract_leafs(ef):
    nodes, outgoing, _ = ef
    for path in nodes:
        lit_id = path[0][1]
        if len(outgoing[path]) == 0 and lit_id > 0:
            yield path
