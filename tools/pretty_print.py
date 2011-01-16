def _pretty_print_node(node, nodemap):
    type, succs, ops = node[:3]
    strops = []
    for op in ops:
        if op[0] == 'node':
            strops.append('$' + str(nodemap[op[1]]))
        elif op[0] == 'const':
            strops.append(repr(op[1]))
        elif op[0] == 'varptr':
            strops.append('&' + str(op[1]))
        else:
            strops.append(str(op[1]))

    if type == 'call' and ops[0][0] != 'oper':
        inst = '%s(%s)' % (strops[0], ', '.join(strops[1:]))
    else:
        inst = '%s %s' % (type, ', '.join(strops))

    return inst

if __name__ == '__main__':
    import sys, json
    real_nodenums = '-r' in sys.argv[1:] or '--real' in sys.argv[1:]

    j = json.load(sys.stdin)

    for fnname, fn in j['cfgs'].iteritems():
        print 'def %s(%s):' % (fnname, ', '.join(fn['params']))

        entry = fn['entry']
        nodes = fn['nodes']
        nodemap = {}
        entry_stack = range(len(nodes))
        entry_stack.append(entry)
        i = 0

        while entry_stack:
            entry = entry_stack.pop()

            nodeq = [entry]
            while nodeq:
                n = nodeq.pop(0)
                if n in nodemap:
                    continue

                while n is not None:
                    nodemap[n] = i
                    i += 1

                    type, succs, ops = nodes[n][:3]
                    n = None
                    for target, index, cond in succs:
                        if index == 0 and cond is None and target not in nodemap:
                            n = target
                        elif target not in nodemap:
                            nodeq.append(target)

        inverse_nodelist = range(len(nodemap))
        for key, value in nodemap.iteritems():
            inverse_nodelist[value] = key

        for i, n in enumerate(inverse_nodelist):
            node = nodes[n]

            succs = sorted([(index, cond, nodemap[target]) for target, index, cond in node[1]])
            strsuccs = []
            separate_next = True
            for index, cond, target in succs:
                condstr = repr(cond) + ' ' if cond is not None else ''
            
                if index == 0 and cond is None and target == i + 1:
                    separate_next = False
                else:
                    if real_nodenums:
                        target = inverse_nodelist[target]
                    if index == 0:
                        strsuccs.append('%s-> $%d' % (condstr, target))
                    else:
                        strsuccs.append('%s->%d $%d' % (condstr, index, target))

            res = '    $%d: %s' % (n if real_nodenums else i,
                _pretty_print_node(node, nodemap if not real_nodenums else { i : i for i in xrange(len(nodes))}))
            if strsuccs:
                res += ' | ' + ', '.join(strsuccs)
            if separate_next:
                res += '\n'
            print res
