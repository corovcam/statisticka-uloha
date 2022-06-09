with open("./insurance.csv", "r") as file:
    with open("./insurance_modified.csv", "w") as file2:
        lines = file.readlines()
        for i in range(1, len(lines)):
            tokens = lines[i].split(',')
            tokens[1] = "1" if tokens[1] == "female" else "0"
            tokens[4] = "1" if tokens[4] == "yes" else "0"
            lines[i] = ",".join(tokens)
        file2.writelines(lines)