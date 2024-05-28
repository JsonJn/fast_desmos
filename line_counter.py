from os import listdir
from os.path import isdir

files_read = 0
directories_read = 0


def file_line_count(path: str) -> int:
    global files_read
    files_read += 1
    with open(path, encoding="utf-8") as file:
        return len(file.readlines())


def line_count(path: str) -> int:
    global directories_read
    directories_read += 1
    files = listdir(path)
    count = sum(file_line_count(f"{path}/{file}") for file in files if file[-3:] == ".rs")
    return count + sum(line_count(f"{path}/{d}") for d in files if isdir(f"{path}/{d}"))


def main():
    print(f"Line count: {line_count("src")}")
    print(f"Read {files_read} files.")
    print(f"Read {directories_read} directories.")


if __name__ == '__main__':
    main()
    input()
