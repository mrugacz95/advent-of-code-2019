import json
import os
from datetime import datetime

import matplotlib.pyplot as plt
from matplotlib import cm
from matplotlib.colors import Normalize
from git import Repo

languges = {
    1: ['.py'],
    2: ['.py'],
    3: ['.py'],
    4: ['.py'],
    5: ['.go'],
    6: ['.js'],
    7: ['.cpp'],
    8: ['.java'],
    9: ['.kt'],
    10: ['.scala'],
    11: ['.groovy'],
    12: ['.php'],
    13: ['.swift'],
    14: ['.cs'],
    15: ['.R'],
    16: ['.ipynb'],
    17: ['.lua'],
    18: ['.lisp'],
    19: ['.m'],
    20: ['.pl'],
    21: ['.rs'],
    22: ['.adb'],
    23: ['.erl'],
    24: ['.dart'],
    25: ['.hs', '.py'],
}

ext_lang = {
    '.py': 'Python',
    '.go': 'Go',
    '.js': 'JavaScript',
    '.cpp': 'C++',
    '.java': 'Java',
    '.kt': 'Kotlin',
    '.scala': 'Scala',
    '.groovy': 'Groovy',
    '.php': 'PHP',
    '.swift': 'Swift',
    '.cs': 'C#',
    '.R': 'R',
    '.ipynb': 'Julia',
    '.lua': 'Lua',
    '.lisp': 'Common Lisp',
    '.m': 'MATLAB/Octave',
    '.pl': 'Perl',
    '.rs': 'Rust',
    '.adb': 'Ada',
    '.erl': 'Erlang',
    '.dart': 'Dart',
    '.hs': 'Haskell',
}


def get_files(dir_name):
    files = os.listdir(dir_name)
    recursive = []
    for file in files:
        subdir = os.path.join(dir_name, file)
        if os.path.isdir(subdir):
            recursive.extend(get_files(subdir))
    found_files = list(map(lambda name: os.path.join(dir_name, name), files)) + recursive
    return found_files


def is_file_in_repo(repo, path_to_file):
    rsub = repo.head.commit.tree
    for element in rsub.traverse():
        if element.path == path_to_file:
            return True
    return False


def commit_dates(repo, file_path):
    for element in repo.tree():
        if element.path == file_path:
            commits = list(repo.iter_commits(paths=element.path))
            return commits[-1].committed_date, commits[0].committed_date


def count_lines(file_path):
    lines = 0
    with open(file_path, 'r') as file:
        for line in file:
            lines += 1
    return lines


def extension(path):
    return os.path.splitext(path)[1]


colors = {
    "Ada": "#02f88c",
    "C#": "#178600",
    "C++": "#f34b7d",
    "Common Lisp": "#3fb68b",
    "Dart": "#00B4AB",
    "Erlang": "#B83998",
    "Go": "#00ADD8",
    "Groovy": "#e69f56",
    "Haskell": "#5e5086",
    "Java": "#b07219",
    "JavaScript": "#f1e05a",
    "Julia": "#a270ba",
    "Kotlin": "#F18E33",
    "Lua": "#000080",
    "MATLAB/Octave": "#e16737",
    "Perl": "#0298c3",
    "PHP": "#4F5D95",
    "Python": "#3572A5",
    "R": "#198CE7",
    "Rust": "#dea584",
    "Scala": "#c22d40",
    "Swift": "#ffac45"
}


def main():
    locs, labels = [], []
    days_duration = []
    last_finish_date = None

    repo = Repo('.')

    plt.style.use('seaborn-darkgrid')
    fig, (ax_top, ax_bot) = plt.subplots(nrows=2, ncols=1)
    fig.set_figwidth(17)
    fig.set_figheight(10)
    for day_num in range(1, 25 + 1):
        dir_path = f"day_{day_num}"
        locs.append(day_num)
        labels.append(day_num)

        files = get_files(dir_path)

        def is_file_relative(path):
            if '/venv/' in path:
                return False
            if path and extension(path) not in languges[day_num]:
                return False
            if not is_file_in_repo(repo, path):
                return False
            return True

        files = list(filter(is_file_relative, files))
        # plot lines of code
        stacked = 0
        for ext in languges[day_num]:
            lines = sum(map(count_lines, filter(lambda f: extension(f) == ext, files)))
            if stacked:
                args = {'bottom': stacked}
            else:
                args = {}
            lang_name = ext_lang[ext]
            ax_top.bar(day_num, lines, label=lang_name, **args, color=colors[lang_name])
            stacked = lines

        start_date, end_date = commit_dates(repo, dir_path)
        start_date, end_date = datetime.fromtimestamp(start_date), datetime.fromtimestamp(end_date)
        if last_finish_date is not None:
            start_date = min(start_date, last_finish_date)
        last_finish_date = end_date
        days_duration.append((end_date - start_date).days + 1)
    # plot duration
    duration_cmap = cm.get_cmap('copper')
    duration_norm = Normalize(vmin=min(days_duration), vmax=max(days_duration))
    for day_num, duration in enumerate(days_duration):
        ax_bot.bar(x=day_num + 1, height=duration, color=duration_cmap(duration_norm(duration)))

    plt.setp(ax_bot, xticks=locs, xticklabels=labels)
    plt.setp(ax_top, xticks=locs, xticklabels=labels)
    handles, labels = ax_top.get_legend_handles_labels()
    by_label = dict(zip(labels, handles))
    ax_top.set_ylabel("Lines of code")
    ax_top.set_xlabel("Day")
    ax_bot.set_ylabel("Completion duration")
    ax_bot.set_xlabel("Day")
    fig.suptitle("Advent of Code summary", fontsize=16)
    ax_top.legend(by_label.values(), by_label.keys(), ncol=8)
    plt.savefig('docs/summary.png')
    plt.show()


if __name__ == '__main__':
    main()
