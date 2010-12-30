def _format_id(id):
    if id[0] == 'func':
        name = id[1]
        pos = name.find('(')
        if pos != -1:
            name = name[:pos]
        return r'\texttt{%s}' % name
    elif id[0] == 'oper':
        return r'\textit{%s}' % id[1]
    elif id[0] == 'node':
        return r'\$%s' % id[1]
    elif id[0] == 'varptr':
        return r'\&%s' % id[1]
    else:
        return str(id[1])

def cfg2dot(fin, fout):
    import json
    src = json.load(fin)['cfgs']

    fout.write('digraph G {\n')
    #fout.write('    node [shape=box];\n')
    fnidx = 0
    for fnname, fn in src.iteritems():
        skipexit2 = True
        for node in fn['nodes']:
            for succ in node[1]:
                if succ[1] != 2 and fn['nodes'][succ[0]][0] == 'exit' and fn['nodes'][succ[0]][2][0][1] == '2':
                    skipexit2 = False
                    break
            if not skipexit2:
                break

        fout.write('    entry%d [label="%s"];\n' % (fnidx, r'\texttt{%s}' % fnname))
        fout.write('    entry%d -> node%d_%d;\n' % (fnidx, fnidx, fn['entry']))
        for i, node in enumerate(fn['nodes']):
            if skipexit2 and node[0] == 'exit' and node[2][0][1] == '2':
                continue

            atomval = ''
            if node[0] == 'call':
                atomval = atomval + _format_id(node[2][0]) + '('
                ops = node[2][1:]
            else:
                atomval = atomval + r'\textbf{%s} ' % node[0]
                ops = node[2]
            atomval = atomval + ', '.join(_format_id(op) for op in ops)
            if node[0] == 'call':
                atomval = atomval + ')'
            fout.write('    node%d_%d [label="%s"];\n' % (fnidx, i, atomval));

            for succ in node[1]:
                if succ[1] == 0:
                    fout.write('    node%d_%d -> node%d_%d [label="%s"];\n' % (fnidx, i, fnidx, succ[0], succ[2]))
                if succ[1] == 1:
                    fout.write('    node%d_%d -> node%d_%d [label="%s",style="dotted"];\n' % (fnidx, i, fnidx, succ[0], xml_escape(succ[2])))
        fnidx += 1
    fout.write('}')

if __name__ == '__main__':
    import argparse
    parser = argparse.ArgumentParser(description='Convert a json-encoded CFG to a dot file.')
    parser.add_argument('infile', nargs='?', default='-', help='The JSON-encoded CFG forest to be checked.');
    args = parser.parse_args()
    
    import sys
    fin = open(args.infile, 'r') if args.infile != '-' else sys.stdin
    cfg2dot(fin, sys.stdout)
