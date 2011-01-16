def delete_nodes(fn, nodes):
    nodes = set(nodes)

    done = False
    while not done:
        done = True
        for i, node in enumerate(fn['nodes']):
            if node[0] == 'phi':
                continue
            for op in node[2]:
                if op[0] == 'node' and op[1] in nodes and i not in nodes:
                    done = False
                    nodes.add(i)

    node_mapping = {}
    for i in xrange(len(fn['nodes'])):
        if i not in nodes:
            node_mapping[i] = len(node_mapping)

    for i in sorted(nodes, reverse=True):
        del fn['nodes'][i]

    for node in fn['nodes']:
        new_succs = []
        for succ in node[1]:
            if succ[0] in node_mapping:
                new_succs.append((node_mapping[succ[0]], succ[1], succ[2]))
        node[1] = new_succs
        node[2] = [op for op in node[2] if op[0] != 'node' or op[1] not in nodes]
        for op in node[2]:
            if op[0] == 'node':
                op[1] = node_mapping[op[1]]

    fn['entry'] = node_mapping[fn['entry']]

def reachable_nodes(cfg, paths):
    entry = cfg['entry']
    nodes = cfg['nodes']

    q = [entry]
    reachable = set(q)
    while q:
        type, succs, ops = nodes[q.pop(0)][:3]
        for target, index, cond in succs:
            if paths and index not in paths:
                continue
            if target in reachable:
                continue
            reachable.add(target)
            q.append(target)
    return reachable

if __name__ == '__main__':
    import argparse
    parser = argparse.ArgumentParser(description='Purge nodes that are not reachable from the entry node.')
    parser.add_argument('-i', '--infile', default='-', help='The JSON-encoded SIR unit to be pruned.')
    parser.add_argument('-o', '--outfile', help='The file to store the pruned SIR unit to.')
    parser.add_argument('fnname', nargs='*', help='The names of subroutines to be pruned.')
    parser.add_argument('-p', '--path', action='append', type=int, help='A path index to be traversed.')
    args = parser.parse_args()

    import sys, json
    fin = open(args.infile, 'r') if args.infile != '-' else sys.stdin
    src = json.load(fin)
    fin.close()

    cfgs = src['cfgs']
    for fnname, fn in cfgs.iteritems():
        if args.fnname and fnname not in args.fnname:
            continue

        r = reachable_nodes(fn, args.path)
        u = set(xrange(len(fn['nodes']))) - r
        delete_nodes(fn, u)

    fout = open(args.outfile, 'w') if args.outfile else open(args.infile, 'w') if args.infile != '-' else sys.stdout
    json.dump(src, fout)
    fout.close()
