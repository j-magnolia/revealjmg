import re

pattern = re.compile("\\$(\\{ *)?((if|for|elsif) *\\( *)?(?P<var>[A-Za-z0-9_-]+)(/[a-z]+( [0-9]+)?)?( *\\))?(\\$| *\\})")

keywords = ("if", "endif", "for", "endfor", "elsif", "else", "endif")

token_pat = re.compile("(?P<token>\\$([^${}]+\\$|\\{[^${}]+\\}))")

def get_vars(txt):
    vars = get_vars_raw(txt)
    vars = list(set(vars))
    vars.sort()
    return vars

def get_vars_raw(txt):
    vars = []
    pointer = 0
    end = len(txt)
    while(pointer < end):
        m = token_pat.search(txt[pointer:])
        if m is not None:
            pointer += m.end()
            token = m.group("token")
            m2 = pattern.match(token)
            if m2 is not None:
                var = m2.group("var")
                if not (var in keywords):
                    vars.append(var)
        else:
            break
    return vars
