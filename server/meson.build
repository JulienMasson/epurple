project('epurple', 'c')
purpledep = dependency('purple')
src =  files('epurple.c', 'emacs.c', 'handlers.c', 'ops.c')
executable('epurple', src, dependencies : purpledep, install : true)
