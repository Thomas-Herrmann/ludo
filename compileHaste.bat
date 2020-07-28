setlocal

set workdir=.haste-work
mkdir %workdir%

copy app\MainJS.hs %workdir%
copy src\LudoJS.hs %workdir%
copy html\ludo.js %workdir%

mkdir haste-out
copy html\ludo.html haste-out

cd %workdir%

hastec "--start=$HASTE_MAIN();" --out=../haste-out/hasteLudo.js --with-js=ludo.js --debug MainJS.hs

