## sed

Serial EDitor



## awk

Print a given column

```bash
$ ls ~/projects/documentation/*.textile
/home/fenton/projects/documentation/dia.textile
/home/fenton/projects/documentation/java.textile
/home/fenton/projects/documentation/secure.textile
/home/fenton/projects/documentation/virtualMachines.textile
```

print one column:

```
$ ls ~/projects/documentation/*.textile | awk -F"/" '{print $(NF)}'
dia.textile
java.textile
secure.textile
virtualMachines.textile
```

Here `-F` means this is the field delimiter, and $(NF), where NF is a
special variable that holds the *number of fields*.

My kill chromium alias:

alias kc='kill `ps -ef | awk "/chromium/ {print \\$2}" | head -1`'
