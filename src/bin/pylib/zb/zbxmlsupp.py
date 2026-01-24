# XML parsing support routines.


def add_value(args, index, value):
    args.extend([None for i in range(index + 1 - len(args))])
    args[index] = value


def qualifiers_match(obj, dest_name, element, args, qualifiers, debug):
    result = True
    if element.getAttribute("alt") != '':
        return False
    for qualifier in qualifiers:
        for attribute in qualifier.keys():
            match_value = qualifier[attribute]
            if attribute == "name":
                value = element.nodeName
            else:
                value = element.getAttribute(attribute)
            if match_value[0] == '#':
                arg_num = int(match_value[1:])
                if debug:
                    print "  ", dest_name, "arg", arg_num, "=>", value
                if value == '':
                    return False
                add_value(args, arg_num, value)
            elif match_value[0] == "*":
                if debug:
                    print "    value", dest_name, "=>", value
                if value == '':
                    return False
                for c in value:
                    # Exclude any Wide_Wide_String values!
                    if ord(c) > 65536:
                        return False
                result = value
            elif match_value[0] == "~":
                if value != '':
                    return False
            elif match_value != value:
                return False
        element = element.parentNode
    return result


def element_value(element):
    result = ""
    for node in element.childNodes:
        if node.nodeType == node.TEXT_NODE:
            result += node.data
    return result


def parse_element(obj, dom, dest_name, qualifiers, debug=False):
    for element in dom.getElementsByTagName(qualifiers[0]["name"]):
        args = []
        match = qualifiers_match(
            obj,
            dest_name,
            element,
            args,
            qualifiers,
            debug
        )
        if match:
            parameter = dest_name.format(*args)
            if type(match).__name__ == 'bool':
                value = element_value(element)
            else:
                value = match
            try:
                obj[parameter] = value
                if debug:
                    print parameter, "=>", value
            except:
                if debug:
                    print "FAIL:", parameter, "=>", value
                pass
        elif debug:
            print dest_name, "not found"
