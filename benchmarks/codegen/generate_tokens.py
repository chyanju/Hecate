if __name__ == '__main__':
    import re

    r = re.compile("val ([A-Z]+):[ ]+(.+ \* )?linenum[ ]+\*[ ]+linenum")

    with open("tokens.sig") as infile:
        lines = "".join(infile.read())
    
    for (token, arg) in r.findall(lines):
        if arg == "":
            print(f"""fun {token}(i,j) = "{token}   " ^ makestring(i:int)""")

    
    #     for line in infile:
    #         token = line.rstrip()
    #         if token 
    #         print(f"""fun {token}(i,j) = "{token}   " ^ makestring(i:int)""")