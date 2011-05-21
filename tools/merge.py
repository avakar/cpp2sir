import json, sys

def _fixup_cfg(cfg, fname_map):
    for tag in cfg.get('tags', []):
        if tag[0] == 'source_range':
            tag[1] = fname_map[tag[1]]
    return cfg

def merge(u_dest, u_src):
    vfn_map2 = u_dest.get('_vfn_map', {})
    for vfn, fn_list in u_src['_vfn_map'].iteritems():
        if vfn not in vfn_map2:
            vfn_map2[vfn] = []
        vfn_map2[vfn].extend(fn_list)

    if '_vfn_param_counts' not in u_dest:
        u_dest['_vfn_param_counts'] = {}
    u_dest['_vfn_param_counts'].update(u_src['_vfn_param_counts'])

    aliases_dest = u_dest.get('aliases', {})
    for fn, aliases in u_src['aliases'].iteritems():
        if fn not in aliases_dest:
            aliases_dest[fn] = []
        aliases_dest[fn] = sorted(list(set(aliases_dest[fn]) | set(aliases)))

    globs = u_dest.get('globals', {})
    globs.update(u_src.get('globals', {}))
    u_dest['globals'] = globs

    fnames = u_dest.get('filenames', [])
    fname_dest_map = { fname: i for i, fname in enumerate(fnames) }
    fname_map = {}
    for i, fname in enumerate(u_src.get('filenames', [])):
        if fname not in fname_dest_map:
            fnames.append(fname)
            fname_dest_map[fname] = len(fnames) - 1
        fname_map[i] = fname_dest_map[fname]

    u_dest['filenames'] = fnames

    cfgs = u_dest.get('cfgs', {})
    for fname, cfg in u_src.get('cfgs', {}).iteritems():
        u_dest['cfgs'][fname] = _fixup_cfg(cfg, fname_map)

u = []
for arg in sys.argv[1:]:
    u.append(json.load(open(arg, 'r')))

while len(u) > 1:
    merge(u[-2], u[-1])
    u.pop()

json.dump(u[0], sys.stdout, sort_keys=True, indent=4)
