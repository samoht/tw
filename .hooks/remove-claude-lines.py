#!/usr/bin/env python3
import sys

def main():
    commit_msg_file = sys.argv[1]
    with open(commit_msg_file, 'r', encoding='utf-8') as f:
        lines = f.readlines()

    filtered_lines = [line for line in lines if 'claude' not in line.lower()]

    with open(commit_msg_file, 'w', encoding='utf-8') as f:
        f.writelines(filtered_lines)

    return 0

if __name__ == '__main__':
    sys.exit(main())
