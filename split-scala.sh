#!/bin/bash -e

export SED=sed
if ! echo | sed -re 's///' &> /dev/null; then
  # Cope with broken OS X sed by switching to GNU sed, as provided by e.g.
  # Homebrew.
  SED=gsed
fi

if [[ -t 1 ]]
then
  columns=$(stty size | cut -d ' ' -f 2)
  let columns--
fi

starting=$(git rev-parse --default HEAD --verify $1)
git update-ref --no-deref -d refs/original/refs/SPLIT_SCALA
git update-ref --no-deref refs/SPLIT_SCALA "$starting"

./git-filter-branch --force \
  --subdirectory-filter 'lc-scala' \
  --msg-filter 'git-filter-branch-msgs' \
  --commit-filter '
      tree=$1
      shift
      parents=$($SED "s/-p//g" <<< "$@")
      showParent() {
          printf -- "-p %s " "$1"
      }
      independentParents() {
        git show-branch --independent $parents
      }
      [ -z "$parents" ] || set -- $(
        firstParent=$(for parent in $parents; do echo $parent; break; done)
        # Warning: using ../message is not part of the interface. But re-reading
        # a file in the shell is enough of a pain that I would rather avoid it.
        if head -1 ../message|grep -q "^Merge pull request"; then
          # For merges of pull requests:
          #
          # Ensure firstParent is always mentioned, so that fast-forward merges
          # into master are not removed, while fast-forwards into other branches
          # are inlined. This matters because index-filter turns many commits into
          # no-ops, creating fast-forward merges.

          for parent in $firstParent $(independentParents|grep -v $firstParent)
          do
            showParent "$parent"
          done
        else
          for parent in $(independentParents)
          do
            showParent "$parent"
          done
        fi
        )
      if test $# = 2 && test "$tree" = $(git rev-parse "$2^{tree}")
      then
        map "$2"
      elif test $# = 4 && test "$tree" = $(git rev-parse "$2^{tree}") && test "$tree" = $(git rev-parse "$4^{tree}")
      then
        map "$2"
      elif test $# = 0 && test "$tree" = "4b825dc642cb6eb9a060e54bf8d69288fbee4904"
      then
        echo
      else
        commit=$(git commit-tree $tree "$@")
        echo -ne "\r\033[0K\r" 1>&2
        git --no-pager log "--pretty=%h %s" -n1 $commit | cut -c1-'$columns' 1>&2
        echo $commit
      fi' -- refs/SPLIT_SCALA

# --date-order is needed to process commits in the right order for msg-filter, see
# https://groups.google.com/d/msg/git-users/aVnbTd9lRrc/gOSmFSN8m4gJ and
# http://article.gmane.org/gmane.comp.version-control.git/217881.
#
# Since however passing --date-order broke since then (in
# https://github.com/git/git/commit/3361a548dbedde96d75bd4134e9ab9e6d82774dd),
# we embed --date-order in our local fork of git-filter-branch.
