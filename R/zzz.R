# Global variable which contains every loaded paper
# .papersManager <- PapersManager();

.pkgGlobalEnv <- new.env(parent=emptyenv());
assign('papersManager', PapersManager(), envir=.pkgGlobalEnv);
