# This file now deprecated

The contents of this file haven been replaced by the recently updated README.md file.

## Old information:

This file documents how to migrate a database from PostgreSQL 12.4 and subversion only
to PostgreSQL 13.1

I will list some of the issues found during devleopment.

## packager role

I noticed this when loading from production into a development
database (pg02).

```
[postgres@pg02 /usr/home/dan/dumping]$ pg_restore -d freshports.prod freshports.org.dump 
pg_restore: while PROCESSING TOC:
pg_restore: from TOC entry 273; 1259 81566 TABLE packages_raw packager
pg_restore: error: could not execute query: ERROR:  role "packager" does not exist
Command was: ALTER TABLE public.packages_raw OWNER TO packager;

pg_restore: warning: errors ignored on restore: 1
```

I think that's wrong. I think `postgres` can own that table. Permissions
should be delegated via roles.

EDIT 2021-02-28 - The above situation is because of a missing role. It is fixed via:

   CREATE ROLE packager;

It is still true that `postgres` will own the table.

The solution for the above: `--no-owner`


## Dump and restore process

This is the current process:

`pg_dump -Fc freshports.org > freshports.org.dump`

Now copy the file to pg02

## create the destination database

`createdb -O postgres -T template0 -E SQL_ASCII freshports.prod`

pg02 has 2 package(s) x 10 core(s) x 2 hardware threads
but we don't use them all

\--no-owner is there to set ownership to postgres

``pg_restore -j 35 --no-owner -d freshports.prod freshports.org.dump``

That fixes the permissions.

Now dump on pg03 (PostgreSQL 13.1):

