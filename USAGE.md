## Rscript? ##

You can run Strategico.R using Rscript command or (for windows users) something like
```
R --slave --no-save --no-restore --no-environ --silent --args --cmd runit < strategico.R
```

## Quick start ##

```
Rscript strategico.R -n sample --cmd create
Rscript strategico.R -n sample --cmd import.csv
Rscript strategico.R -n sample --cmd eval.items
Rscript strategico.R -n sample --cmd export.csv
```

## Empty filesystem ##

```
Rscript strategico.R --cmd empty.fs -n sample
```

## Empty DB tables ##

```
Rscript strategico.R --cmd empty.db -n sample
```

## Eval/Run Engine ##
```
./strategico.R --project.name=sample --cmd=eval.items --id.list 2,3,4 --item.values=V1,V2 -p "try.models=c('mean')"

./strategico.R --project.name=sample --cmd=eval.items --id.range 1:4 --item.values=V1,V2 -p "try.models=c('mean')"

./strategico.R --cmd eval.children -n sample --id.list "16"

```

## Eval/Run engine (dbsummary records/items with Run=1) ##
```
./strategico.R --project.name=sample --cmd=eval.items.from.db -v V1
```

## Eval/Run engine for a generic (unknown item) TS ##
```
./strategico.R --project.name=sample --cmd=eval.ts --ts.string=1,2,32,32,32,33,32,32,32,32,32,3,6,7,8,8,99,9,90 --ts.start=2001-1 --ts.freq=2 -i 4343 -p "try.models=c('mean')"
```

## Export DB to CSV ##

```
./strategico.R --cmd export.csv -n sample
```

## Export reports ##

```
./strategico.R --cmd build.suspicious -n sample
```

```
./strategico.R --cmd build.stats -n sample
```

## Runit ##

```
./strategico.R --cmd=runit
```