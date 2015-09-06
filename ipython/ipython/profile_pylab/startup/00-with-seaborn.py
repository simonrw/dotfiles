from __future__ import print_function

try:
    print('Loading seaborn...', end=' ')
    import seaborn as sns
except ImportError:
    print('Cannot load seaborn')
else:
    sns.set()
    print('Done')
