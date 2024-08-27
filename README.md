This repo contains the upgrade instructions to take a production FreshPorts
database and convert it to use git.

The original instructions are at https://news.freshports.org/2021/01/01/git-database-upgrade-process/

This is the short version of that.

1. On the source server:

       pg_dump -Fc freshports.org -f freshports.org.dump

1. on the destination server:

       sudo su -l postgres
       createdb -O postgres -T template0 -E SQL_ASCII freshports.prod
       pg_restore -j 20 --no-owner -d freshports.prod freshports.org.dump 

1. After restore:

       begin;
       revoke all PRIVILEGES ON all tables in schema pg_catalog from rsyncer;
       create extension pgcrypto      from unpackaged;
       create extension fuzzystrmatch from unpackaged;
       create extension plperl        from unpackaged;

 Then commit if all OK.

1. Update schema etc

       begin;
       \i updates-2020-12-git-changes.ddl
       \i updates-2020-12-git-changes.sql
       \i updates-2021-02-git-changes.ddl
       \i updates-2021-02.27-git-changes.ddl

 Then commit if all OK.

1. Adjust datatypes, relational integrity, and functions

       begin;
       \i datatype.txt
       \i ri.txt
       \i sp.txt

 Then commit if all OK.
