project('epurple', 'c')
purpledep = dependency('purple')
src =  files('epurple.c', 'emacs.c', 'handlers.c', 'log.c', 'ops.c', 'protocol.c')
executable('epurple', src, dependencies : purpledep, install : true)
