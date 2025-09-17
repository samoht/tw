#!/usr/bin/env python3
import sys
import re

def has_emoji(text):
    # Unicode ranges for emojis
    emoji_pattern = re.compile(
        "["
        "\U0001F600-\U0001F64F"  # emoticons
        "\U0001F300-\U0001F5FF"  # symbols & pictographs
        "\U0001F680-\U0001F6FF"  # transport & map symbols
        "\U0001F1E0-\U0001F1FF"  # flags (iOS)
        "\U00002702-\U000027B0"  # dingbats
        "\U000024C2-\U0001F251"
        "]+", flags=re.UNICODE)
    return bool(emoji_pattern.search(text))

def main():
    commit_msg_file = sys.argv[1]
    with open(commit_msg_file, 'r', encoding='utf-8') as f:
        lines = f.readlines()

    # Check for emojis in the commit message
    commit_text = ''.join(lines)
    if has_emoji(commit_text):
        print("Error: Commit message contains emojis, which are not allowed.", file=sys.stderr)
        return 1

    filtered_lines = [line for line in lines if 'claude' not in line.lower()]

    with open(commit_msg_file, 'w', encoding='utf-8') as f:
        f.writelines(filtered_lines)

    return 0

if __name__ == '__main__':
    sys.exit(main())
