# Get info on current git status for gitinfo2
# That package recommends creating the appropriate file via git hooks; however,
# that would require everyone working on the repository to install the hooks.
# Creating the auxiliary file in the Makefile instead assures that it's always
# there when the document is compiled.
FIRSTTAG := $(shell git describe --tags --always --dirty='-*' 2>/dev/null)
RELTAG := $(shell git describe --tags --long --always --dirty='-*' --match '[0-9]*.*' 2>/dev/null)
gitHeadLocal.gin:
	git --no-pager log -1 --date=short --decorate=short --pretty=format:"\usepackage[shash={%h}\
, lhash={%H}\
, authname={%an}\
, authemail={%ae}\
, authsdate={%ad}\
, authidate={%ai}\
, authudate={%at}\
, commname={%cn}\
, commemail={%ce}\
, commsdate={%cd}\
, commidate={%ci}\
, commudate={%ct}\
, refnames={%d}\
, firsttagdescribe={$(FIRSTTAG)}\
, reltag={$(RELTAG)}\
]{gitexinfo} " HEAD > gitHeadLocal.gin
