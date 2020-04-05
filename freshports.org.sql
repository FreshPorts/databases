--
-- PostgreSQL database dump
--

-- Dumped from database version 12.2
-- Dumped by pg_dump version 12.2

SET statement_timeout = 0;
SET lock_timeout = 0;
SET idle_in_transaction_session_timeout = 0;
SET client_encoding = 'SQL_ASCII';
SET standard_conforming_strings = on;
SELECT pg_catalog.set_config('search_path', '', false);
SET check_function_bodies = false;
SET xmloption = content;
SET client_min_messages = warning;
SET row_security = off;

--
-- Name: session_variables; Type: SCHEMA; Schema: -; Owner: dan
--

CREATE SCHEMA session_variables;


ALTER SCHEMA session_variables OWNER TO dan;

--
-- Name: plperl; Type: PROCEDURAL LANGUAGE; Schema: -; Owner: dan
--

CREATE OR REPLACE PROCEDURAL LANGUAGE plperl;


ALTER PROCEDURAL LANGUAGE plperl OWNER TO dan;

--
-- Name: LANGUAGE plperl; Type: COMMENT; Schema: -; Owner: dan
--

COMMENT ON LANGUAGE plperl IS 'PL/Perl procedural language';


--
-- Name: also_watched_record; Type: TYPE; Schema: public; Owner: dan
--

CREATE TYPE public.also_watched_record AS (
	element_id integer,
	url text
);


ALTER TYPE public.also_watched_record OWNER TO dan;

--
-- Name: commit_record; Type: TYPE; Schema: public; Owner: dan
--

CREATE TYPE public.commit_record AS (
	commit_log_id integer,
	commit_date_raw timestamp with time zone,
	message_subject text,
	message_id text,
	committer text,
	commit_description text,
	commit_date text,
	commit_time text,
	encoding_losses boolean,
	port_id integer,
	needs_refresh smallint,
	forbidden text,
	broken text,
	deprecated text,
	ignore text,
	element_id integer,
	version text,
	revision text,
	epoch text,
	date_added integer,
	short_description text,
	category_id integer,
	port text,
	status character(1),
	category text,
	vulnerable_current integer,
	vulnerable_past integer,
	restricted text,
	no_cdrom text,
	expiration_date date,
	is_interactive text,
	only_for_archs text,
	not_for_archs text,
	repo_name text,
	svn_revision text,
	svn_hostname text,
	path_to_repo text,
	watch bigint,
	element_pathname text
);


ALTER TYPE public.commit_record OWNER TO dan;

--
-- Name: conflicts; Type: TYPE; Schema: public; Owner: dan
--

CREATE TYPE public.conflicts AS ENUM (
    'conflicts',
    'conflicts_build',
    'conflicts_install'
);


ALTER TYPE public.conflicts OWNER TO dan;

--
-- Name: element_id_pathname; Type: TYPE; Schema: public; Owner: dan
--

CREATE TYPE public.element_id_pathname AS (
	element_id integer,
	pathname text
);


ALTER TYPE public.element_id_pathname OWNER TO dan;

--
-- Name: element_type; Type: TYPE; Schema: public; Owner: dan
--

CREATE TYPE public.element_type AS (
	id integer,
	name text,
	type text,
	status text,
	iscategory boolean,
	isport boolean,
	element_pathname text
);


ALTER TYPE public.element_type OWNER TO dan;

--
-- Name: freshports_commit_record; Type: TYPE; Schema: public; Owner: dan
--

CREATE TYPE public.freshports_commit_record AS (
	port_id integer,
	short_description text,
	date_added timestamp with time zone,
	needs_refresh smallint,
	category text,
	category_id integer,
	message_id text,
	committer text,
	commit_description text,
	commit_log_id integer,
	port text,
	commit_date text,
	commit_date_raw timestamp with time zone,
	commit_time text,
	encoding_losses boolean,
	status character(1),
	version text,
	revision text,
	epoch text,
	version_name text,
	change_type text,
	element_pathname text,
	is_port boolean,
	element_id integer,
	forbidden text,
	deprecated text,
	ignore text,
	broken text,
	vulnerable_current integer,
	vulnerable_past integer,
	restricted text,
	expiration_date date,
	no_cdrom text,
	is_interactive text,
	only_for_archs text,
	not_for_archs text,
	svn_revision text,
	svn_hostname text,
	path_to_repo text,
	repo_name text,
	onwatchlist bigint
);


ALTER TYPE public.freshports_commit_record OWNER TO dan;

--
-- Name: getportfrompackagename_record; Type: TYPE; Schema: public; Owner: dan
--

CREATE TYPE public.getportfrompackagename_record AS (
	port_id integer,
	category text,
	port text,
	element_pathname text
);


ALTER TYPE public.getportfrompackagename_record OWNER TO dan;

--
-- Name: logincounts_record; Type: TYPE; Schema: public; Owner: dan
--

CREATE TYPE public.logincounts_record AS (
	date date,
	count integer
);


ALTER TYPE public.logincounts_record OWNER TO dan;

--
-- Name: mylogincounts_record; Type: TYPE; Schema: public; Owner: dan
--

CREATE TYPE public.mylogincounts_record AS (
	date date,
	count integer
);


ALTER TYPE public.mylogincounts_record OWNER TO dan;

--
-- Name: old_new_ports_record; Type: TYPE; Schema: public; Owner: dan
--

CREATE TYPE public.old_new_ports_record AS (
	name_old text,
	category_old text,
	name_new text,
	category_new text
);


ALTER TYPE public.old_new_ports_record OWNER TO dan;

--
-- Name: packageflavors_record; Type: TYPE; Schema: public; Owner: dan
--

CREATE TYPE public.packageflavors_record AS (
	id bigint,
	port_id bigint,
	flavor_id bigint,
	flavor_name text,
	name text,
	flavor_number integer
);


ALTER TYPE public.packageflavors_record OWNER TO dan;

--
-- Name: vuxml_record; Type: TYPE; Schema: public; Owner: dan
--

CREATE TYPE public.vuxml_record AS (
	id integer,
	vid text,
	package_name text,
	op1 text,
	v1 text,
	op2 text,
	v2 text
);


ALTER TYPE public.vuxml_record OWNER TO dan;

--
-- Name: allfilesdeleted(integer); Type: FUNCTION; Schema: public; Owner: dan
--

CREATE FUNCTION public.allfilesdeleted(integer) RETURNS integer
    LANGUAGE plpgsql STABLE
    AS $_$
	DECLARE
		p_element_id   		ALIAS FOR $1;

		queries				RECORD;
		l_result			int;

BEGIN
	FOR queries IN 
		SELECT id, status, directory_file_flag
	      FROM element
    	 WHERE parent_id = p_element_id
	LOOP
		IF queries.directory_file_flag = 'F' THEN
			IF queries.status = 'A' THEN
				return 0;
			END IF;
		ELSE
			l_result := AllFilesDeleted(queries.id);
			IF l_result = 0 THEN
				return 0;
			END IF;
		END IF;

	END LOOP;

	return 1;
END;
$_$;


ALTER FUNCTION public.allfilesdeleted(integer) OWNER TO dan;

--
-- Name: announcementsget(); Type: FUNCTION; Schema: public; Owner: dan
--

CREATE FUNCTION public.announcementsget() RETURNS SETOF text
    LANGUAGE sql STABLE
    AS $$
   SELECT text
	  FROM announcements
	 WHERE start_date <= current_date
	   AND ((end_date IS NULL) OR (end_date >= current_date));
$$;


ALTER FUNCTION public.announcementsget() OWNER TO dan;

--
-- Name: announcementsgetplain(); Type: FUNCTION; Schema: public; Owner: dan
--

CREATE FUNCTION public.announcementsgetplain() RETURNS SETOF text
    LANGUAGE sql STABLE
    AS $$
   SELECT text_plain
	  FROM announcements
	 WHERE start_date <= current_date
	   AND ((end_date IS NULL) OR (end_date >= current_date));
$$;


ALTER FUNCTION public.announcementsgetplain() OWNER TO dan;

--
-- Name: armor(bytea); Type: FUNCTION; Schema: public; Owner: dan
--

CREATE FUNCTION public.armor(bytea) RETURNS text
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/pgcrypto', 'pg_armor';


ALTER FUNCTION public.armor(bytea) OWNER TO dan;

--
-- Name: categories_insert_update(); Type: FUNCTION; Schema: public; Owner: dan
--

CREATE FUNCTION public.categories_insert_update() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
   BEGIN
   	IF new.element_id IS NULL AND new.is_primary THEN
         RAISE EXCEPTION 'if is_primary is set, then element_id must be non-null for a category (name=% description=%', new.name, new.description;
      end if;

   	IF new.element_id IS NOT NULL AND NOT new.is_primary THEN
         RAISE EXCEPTION 'Non primary categories must not have a element_id (name=% description=%', new.name, new.description;
      end if;

      RETURN new;
   END;$$;


ALTER FUNCTION public.categories_insert_update() OWNER TO dan;

--
-- Name: categories_new(); Type: FUNCTION; Schema: public; Owner: dan
--

CREATE FUNCTION public.categories_new() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
   BEGIN
		NOTIFY category_new;

		RETURN new;
   END;
$$;


ALTER FUNCTION public.categories_new() OWNER TO dan;

--
-- Name: categoryport(integer); Type: FUNCTION; Schema: public; Owner: dan
--

CREATE FUNCTION public.categoryport(integer) RETURNS text
    LANGUAGE sql STABLE
    AS $_$
  SELECT C.name || '/' || E.name
    FROM ports P, categories C, element E
   WHERE P.id          = $1
     AND P.category_id = C.id
     AND P.element_id  = E.id;
$_$;


ALTER FUNCTION public.categoryport(integer) OWNER TO dan;

--
-- Name: categoryportcount(text); Type: FUNCTION; Schema: public; Owner: dan
--

CREATE FUNCTION public.categoryportcount(text) RETURNS bigint
    LANGUAGE sql STABLE
    AS $_$
        SELECT count(*)
          FROM categories C 
          JOIN ports_categories PC ON C.id         = PC.category_id
          JOIN ports            P  ON PC.port_id   = P.id
          JOIN element          E  ON P.element_id = E.id
          JOIN element_pathname EP on E.id         = EP.element_id
         WHERE C.name   = $1
           AND E.status = 'A'
           AND EP.pathname like '/ports/head/%';
$_$;


ALTER FUNCTION public.categoryportcount(text) OWNER TO dan;

--
-- Name: categoryportcount(text, text); Type: FUNCTION; Schema: public; Owner: dan
--

CREATE FUNCTION public.categoryportcount(text, text) RETURNS bigint
    LANGUAGE sql STABLE
    AS $_$
        SELECT count(*)
          FROM categories C 
          JOIN ports_categories PC ON C.id         = PC.category_id
          JOIN ports            P  ON PC.port_id   = P.id
          JOIN element          E  ON P.element_id = E.id
          JOIN element_pathname EP on E.id         = EP.element_id
         WHERE C.name   = $1
           AND E.status = 'A'
           AND EP.pathname like '/ports/branches/' || $2 || '%';
$_$;


ALTER FUNCTION public.categoryportcount(text, text) OWNER TO dan;

--
-- Name: categorystatsupdate(); Type: FUNCTION; Schema: public; Owner: dan
--

CREATE FUNCTION public.categorystatsupdate() RETURNS integer
    LANGUAGE plpgsql
    AS $$
BEGIN
 DELETE FROM category_stats;
 INSERT INTO category_stats (
  SELECT categories.id               AS category_id,
         count(ports_active.id)      AS count,
         max(commit_log.commit_date) AS updated
         
    FROM categories, ports_active left outer join commit_log on ( ports_active.last_commit_id = commit_log.id )
   WHERE categories.id   = ports_active.category_id
     AND categories.is_primary
GROUP BY categories.id, categories.name, categories.description, is_primary, categories.element_id
UNION
  SELECT categories.id               AS category_id,
         count(ports_active.id)      AS count,
         max(commit_log.commit_date) AS updated
    FROM ports_categories, categories, ports_active left outer join commit_log on ( ports_active.last_commit_id = commit_log.id )
   WHERE ports_active.id = ports_categories.port_id
     AND categories.id   = ports_categories.category_id
     AND NOT categories.is_primary
GROUP BY categories.id, categories.name, categories.description, is_primary, categories.element_id);

	return 1;
END;
$$;


ALTER FUNCTION public.categorystatsupdate() OWNER TO dan;

--
-- Name: check_last_commit_id(); Type: FUNCTION; Schema: public; Owner: dan
--

CREATE FUNCTION public.check_last_commit_id() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
   declare 
      l_max_commit_date  timestamp with time zone;
      l_commit_log_id    integer;
begin

  if new.last_commit_id is null then
    SELECT max(CL.commit_date)
      INTO l_max_commit_date
      FROM commit_log CL, commit_log_ports CLP
     WHERE CLP.port_id = NEW.id
       AND CL.id       = CLP.commit_log_id;

    IF FOUND THEN
      SELECT CL.id
        INTO l_commit_log_id
        FROM commit_log CL, commit_log_ports CLP
       WHERE CLP.port_id    = NEW.id
         AND CL.id          = CLP.commit_log_id
         AND CL.commit_date = l_max_commit_date;

      IF FOUND THEN
        NEW.last_commit_id := l_commit_log_id;
      END IF;
    END IF;

  end if;

  RETURN NEW;

end;
$$;


ALTER FUNCTION public.check_last_commit_id() OWNER TO dan;

--
-- Name: check_parent(); Type: FUNCTION; Schema: public; Owner: dan
--

CREATE FUNCTION public.check_parent() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
   declare
      ElementCount int4;
begin
   --  message 'check_parent';
   if new.parent_id is not null then
      --    message 'check_parent 2';
      select count(*) 
        into ElementCount
        from element
       where element.id = new.parent_id;
      --    message 'check_parent 3';
      if (ElementCount = 0) then
         -- message 'no such parent ' || NOW(*);
         RAISE EXCEPTION 'modifications to element % with name=% cannot be completed as the parent % does not exist.', new.id, new.name, new.parent_id;
      end if;
      if new.parent_id = new.id then
         RAISE EXCEPTION 'modifications to element % with name=% cannot be completed as the element cannot be a child of itself.', new.id, new.name;
      end if;
   end if;
   --    message 'check_parent 4'

   RETURN NEW;
end;
$$;


ALTER FUNCTION public.check_parent() OWNER TO dan;

--
-- Name: check_siblings(); Type: FUNCTION; Schema: public; Owner: dan
--

CREATE FUNCTION public.check_siblings() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
   declare 
      ElementCount integer;
		PortCount		integer;
		OldParentStatus	text;
begin

  if new.Parent_ID is null then
    select count(*) 
      into ElementCount
      from element
     where element.name=new.name
       and element.Parent_ID is null
     group by element.name;
  else
    select count(*) 
      into ElementCount
      from element
     where element.name=new.name
       and element.Parent_ID=new.Parent_ID
     group by element.name;
  end if;

  if (ElementCount > 1) then
    RAISE EXCEPTION 'element % cannot be created as it already exists under that parent %', new.name, new.parent_id;
  end if;

	-- are we dealing with a Makefile which has a parent?

	if new.name = 'Makefile' and new.Parent_ID is NOT NULL then

		-- is this Makefile part of a port?
		SELECT count(*)
		  INTO PortCount
		  FROM ports
		 WHERE element_id = new.Parent_ID;

		IF PortCount > 0 THEN


		-- we did a truth table.  The status of a port is
		-- determined by the status of the Makefile.
		-- if it is deleted, we delete the port.
		-- and if the Makefile is put back, we activate the port

			SELECT status
			  INTO oldParentStatus
			  FROM element
			 WHERE id = new.Parent_ID;

			if oldParentStatus != new.status then
				UPDATE element
				   SET status = new.status
				 WHERE id = new.Parent_ID;
			end if;
		end if;
	end if;

  RETURN NEW;

end;
$$;


ALTER FUNCTION public.check_siblings() OWNER TO dan;

--
-- Name: clearslaveport(text); Type: FUNCTION; Schema: public; Owner: dan
--

CREATE FUNCTION public.clearslaveport(text) RETURNS integer
    LANGUAGE plpgsql
    AS $_$
	DECLARE
		p_SlaveCategoryPort	ALIAS FOR $1;
	
		position			int;
		l_SlavePortCategory	text;
		l_SlavePortName		text;
		l_SlavePortID		int4;

	BEGIN
		position := strpos(p_SlaveCategoryPort, '/');
		l_SlavePortCategory := substr(p_SlaveCategoryPort, 1, position - 1);
		l_SlavePortName     := substr(p_SlaveCategoryPort, position + 1);

		l_SlavePortID := GetPort(l_SlavePortCategory, l_SlavePortName);
		
		DELETE FROM master_slave_ports
		WHERE  slave_port_id = l_SlavePortID;

		UPDATE ports
		   SET is_slave_port = FALSE
		 WHERE id            = l_SlavePortID;

		return l_SlavePortID;
	END
$_$;


ALTER FUNCTION public.clearslaveport(text) OWNER TO dan;

--
-- Name: commit_log_delete_check(); Type: FUNCTION; Schema: public; Owner: dan
--

CREATE FUNCTION public.commit_log_delete_check() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
begin

	-- if we are removing a commit_log, make sure we remove
	-- the commit_log_elements as well

	delete
      from commit_log_elements
     where commit_log_id = old.id;

   RETURN OLD;
end;
$$;


ALTER FUNCTION public.commit_log_delete_check() OWNER TO dan;

--
-- Name: commit_log_insert(); Type: FUNCTION; Schema: public; Owner: dan
--

CREATE FUNCTION public.commit_log_insert() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
BEGIN
   INSERT INTO latest_commits (commit_log_id, commit_date) values (new.id,  new.commit_date);

   -- see also commit_log_update
   INSERT INTO cache_clearing_dates (date_to_clear) 
     SELECT new.commit_date
     WHERE NOT EXISTS (SELECT date_to_clear FROM cache_clearing_dates WHERE date_to_clear = new.commit_date);

   IF FOUND THEN
     NOTIFY date_updated;
   END IF;

   RETURN new;
END;$$;


ALTER FUNCTION public.commit_log_insert() OWNER TO dan;

--
-- Name: commit_log_ports_insert(); Type: FUNCTION; Schema: public; Owner: dan
--

CREATE FUNCTION public.commit_log_ports_insert() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
BEGIN
   INSERT INTO latest_commits_ports (commit_log_id, commit_date)
	      SELECT new.commit_log_id,
            (SELECT commit_log.commit_date
               FROM commit_log
              WHERE (commit_log.id = new.commit_log_id)
            )
       WHERE (NOT (EXISTS (SELECT commits_latest_ports.commit_log_id
                             FROM latest_commits_ports commits_latest_ports
                            WHERE (commits_latest_ports.commit_log_id = new.commit_log_id)
                           )
                   )
             );
   RETURN new;
END;$$;


ALTER FUNCTION public.commit_log_ports_insert() OWNER TO dan;

--
-- Name: commit_log_ports_insert(integer); Type: FUNCTION; Schema: public; Owner: dan
--

CREATE FUNCTION public.commit_log_ports_insert(integer) RETURNS boolean
    LANGUAGE plpgsql
    AS $_$
DECLARE
	CommitLogID	ALIAS for $1;
BEGIN
   INSERT INTO latest_commits_ports (commit_log_id, commit_date)
	      SELECT CommitLogID, 
            (SELECT commit_log.commit_date
               FROM commit_log
              WHERE (commit_log.id = CommitLogID)
            )
       WHERE (NOT (EXISTS (SELECT commits_latest_ports.commit_log_id
                             FROM latest_commits_ports commits_latest_ports
                            WHERE (commits_latest_ports.commit_log_id = CommitLogID)
                           )
                   )
             );

   RETURN FOUND;
END;$_$;


ALTER FUNCTION public.commit_log_ports_insert(integer) OWNER TO dan;

--
-- Name: commit_log_ports_vuxml_clear_cache(); Type: FUNCTION; Schema: public; Owner: dan
--

CREATE FUNCTION public.commit_log_ports_vuxml_clear_cache() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
   BEGIN
      IF TG_OP = 'INSERT' OR TG_OP = 'UPDATE' THEN
         INSERT INTO cache_clearing_ports(port_id, category, port)
             SELECT PA.id, PA.category, PA.name
               FROM ports_all PA
              WHERE PA.id = NEW.port_id;

         NOTIFY port_updated;
      END IF;

      IF TG_OP = 'DELETE' THEN
         INSERT INTO cache_clearing_ports(port_id, category, port)
            SELECT PA.id, PA.category, PA.name
              FROM ports_all PA
             WHERE PA.id = OLD.port_id;

         NOTIFY port_updated;
      END IF;
      RETURN NEW;
   END
$$;


ALTER FUNCTION public.commit_log_ports_vuxml_clear_cache() OWNER TO dan;

--
-- Name: commit_log_ports_vuxml_purge(text); Type: FUNCTION; Schema: public; Owner: dan
--

CREATE FUNCTION public.commit_log_ports_vuxml_purge(text) RETURNS SETOF integer
    LANGUAGE sql
    AS $_$
DELETE FROM commit_log_ports_vuxml CLPV
 USING vuxml V
 WHERE V.id  = CLPV.vuxml_id
   AND V.vid = $1
RETURNING 1
$_$;


ALTER FUNCTION public.commit_log_ports_vuxml_purge(text) OWNER TO dan;

--
-- Name: commit_log_update(); Type: FUNCTION; Schema: public; Owner: dan
--

CREATE FUNCTION public.commit_log_update() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
BEGIN
   UPDATE latest_commits_ports
      SET commit_date = new.commit_date
    WHERE (latest_commits_ports.commit_log_id = new.id);

   UPDATE latest_commits
      SET commit_date = new.commit_date
    WHERE (latest_commits.commit_log_id = new.id);

   -- see also commit_log_insert
   INSERT INTO cache_clearing_dates (date_to_clear) 
     SELECT new.commit_date
     WHERE NOT EXISTS (SELECT date_to_clear FROM cache_clearing_dates WHERE date_to_clear = new.commit_date);
   IF FOUND THEN
     NOTIFY date_updated;
   END IF;

   RETURN new;
END;$$;


ALTER FUNCTION public.commit_log_update() OWNER TO dan;

--
-- Name: commitlogbranchesinsert(integer, integer); Type: FUNCTION; Schema: public; Owner: dan
--

CREATE FUNCTION public.commitlogbranchesinsert(integer, integer) RETURNS boolean
    LANGUAGE plpgsql
    AS $_$
    DECLARE
        l_CommitLogId  ALIAS for $1;
        l_BranchId     ALIAS for $2;

    BEGIN
        INSERT INTO commit_log_branches (commit_log_id, branch_id) values (l_CommitLogId, l_BranchId);
        IF FOUND THEN
            RETURN TRUE;
        ELSE
            RETURN FALSE;
        END IF;
   END;
$_$;


ALTER FUNCTION public.commitlogbranchesinsert(integer, integer) OWNER TO dan;

--
-- Name: confirmuseraccount(text); Type: FUNCTION; Schema: public; Owner: dan
--

CREATE FUNCTION public.confirmuseraccount(text) RETURNS integer
    LANGUAGE plpgsql
    AS $_$

-- return values:
--  0 : no such token found
-- -1 : some error occurred
--  1 : confirmed and enabled
--  2 : more than one user found with that token
--      very unlikely to occur.

	DECLARE
 		TheToken		ALIAS for $1;

		RowsModified	int4;
		Result			int4;


    BEGIN

		UPDATE users
		   SET status  = 'A'
		 WHERE status != 'D'
	AND EXISTS (
			SELECT user_id 
		      FROM user_confirmations
		     WHERE token   = TheToken
		       AND user_id = users.id
			);

		GET DIAGNOSTICS RowsModified = ROW_COUNT;

		IF RowsModified = 1 THEN
			Result = 1;
		ELSE
			IF RowsModified = 0 THEN
				Result = 0;
			ELSE
				IF RowsModified > 1 THEN
					Result = 2;
				ELSE
					Result = -1;
				END IF;
			END IF;
		END IF;

		RETURN Result;				
		
    END;
$_$;


ALTER FUNCTION public.confirmuseraccount(text) OWNER TO dan;

--
-- Name: copyportfilesfromheadtobranch(text, text, text); Type: FUNCTION; Schema: public; Owner: dan
--

CREATE FUNCTION public.copyportfilesfromheadtobranch(text, text, text) RETURNS integer
    LANGUAGE plpgsql
    AS $_$
DECLARE
  a_category_name ALIAS for $1;
  a_port_name     ALIAS for $2;
  a_branch_name   ALIAS for $3;
 
  l_r                      element_id_pathname%rowtype;
  l_element_id_of_new_port int4;

  BEGIN

  -- this query will add the new port... the key is the element_add() function call

  FOR l_r IN
  WITH RECURSIVE all_descendents AS ( 
  SELECT id, name, parent_id, directory_file_flag, status
    FROM element
    WHERE id = (select pathname_id('/ports/head/' || a_category_name || '/' || a_port_name || '/'))
  UNION
  SELECT E.id, E.name, E.parent_id, E.directory_file_flag, E.status
    FROM element E
    JOIN all_descendents AD
      ON (E.parent_id = AD.id)
  )
    SELECT element_add(replace(element_pathname(id), '/ports/head/', '/ports/branches/' || a_branch_name || '/'), directory_file_flag)
      FROM all_descendents
     WHERE status = 'A'
  LOOP
  END LOOP;

  IF FOUND THEN
    -- based on the element_id of the pathname for newly created port, grab the port id
     SELECT pathname_id('/ports/branches/' || a_branch_name || '/' || a_category_name || '/' || a_port_name || '/')
      INTO l_element_id_of_new_port;

  END IF;

  return l_element_id_of_new_port;

  END;
$_$;


ALTER FUNCTION public.copyportfilesfromheadtobranch(text, text, text) OWNER TO dan;

--
-- Name: createcategory(text, text, boolean); Type: FUNCTION; Schema: public; Owner: dan
--

CREATE FUNCTION public.createcategory(text, text, boolean) RETURNS integer
    LANGUAGE plpgsql
    AS $_$
   DECLARE 
      category_name        ALIAS for $1;
      category_description ALIAS for $2;
      category_is_primary  ALIAS for $3;

      pathname             text;
      category_element_id  int4;
      category_id          int4;

   BEGIN
    category_id := nextval('categories_id_seq');

   	IF category_is_primary THEN
	   pathname            := 'ports/' || category_name;
   	   category_element_id := Pathname_ID(pathname);

       IF category_element_id is NULL THEN
         	category_element_id := Element_Add(pathname, 'D');
       END IF;

   	   INSERT INTO categories (id,          is_primary,          element_id,          name,          description)
      	            values     (category_id, category_is_primary, category_element_id, category_name, category_description);
	ELSE
   	   INSERT INTO categories (id,          is_primary,          name,          description)
      	            values     (category_id, category_is_primary, category_name, category_description);
		END IF;

      return category_id;

   END;
$_$;


ALTER FUNCTION public.createcategory(text, text, boolean) OWNER TO dan;

--
-- Name: createport(integer, integer); Type: FUNCTION; Schema: public; Owner: dan
--

CREATE FUNCTION public.createport(integer, integer) RETURNS integer
    LANGUAGE plpgsql
    AS $_$
	DECLARE 	
		port_element_id		ALIAS for $1;
		port_category_id	ALIAS for $2;

		port_id				int4;

	BEGIN
		-- this just creates the bare minimum for a port.
		-- there are many other fields which need to be populated.

		port_id := nextval('ports_id_seq');
		INSERT INTO ports (id, element_id, category_id)
				values     (port_id, port_element_id, port_category_id);

		return port_id;

	END;
$_$;


ALTER FUNCTION public.createport(integer, integer) OWNER TO dan;

--
-- Name: createport(text, text, text); Type: FUNCTION; Schema: public; Owner: dan
--

CREATE FUNCTION public.createport(text, text, text) RETURNS integer
    LANGUAGE plpgsql
    AS $_$
DECLARE
  a_category_name ALIAS for $1;
  a_port_name     ALIAS for $2;
  a_branch_name   ALIAS for $3;
 
  l_element_id_of_new_port int4;
  l_port_id_branch         int4;
  l_port_id_head           int4;
 
  BEGIN
 
  SELECT CopyPortFilesFromHeadToBranch(a_category_name, a_port_name, a_branch_name)
    INTO l_element_id_of_new_port;
 
  IF FOUND THEN
    SELECT createport(l_element_id_of_new_port, getcategory(a_category_name))
      INTO l_port_id_branch;

    SELECT PA.id
      FROM ports_active PA
     WHERE PA.element_id = l_element_id_of_new_port
      INTO l_port_id_branch;

    -- we need to set up the port_categories table first...
    -- why? Can't this be set up during refresh of the port?

    RAISE WARNING 'about to call GetPort and I only want stuff from head, so branch agnostic is OK.';

    -- we need head on this call, because we are copying from head to branch
    l_port_id_head = GetPort(a_category_name, a_port_name);
    INSERT INTO ports_categories(port_id, category_id)
     SELECT l_port_id_branch, PC.category_id
       FROM ports_categories PC
      WHERE PC.port_id = l_port_id_head;

  END IF;
  
  RETURN l_port_id_branch;
 
  END;
$_$;


ALTER FUNCTION public.createport(text, text, text) OWNER TO dan;

--
-- Name: crypt(text, text); Type: FUNCTION; Schema: public; Owner: dan
--

CREATE FUNCTION public.crypt(text, text) RETURNS text
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/pgcrypto', 'pg_crypt';


ALTER FUNCTION public.crypt(text, text) OWNER TO dan;

--
-- Name: currenttimestamputc(); Type: FUNCTION; Schema: public; Owner: dan
--

CREATE FUNCTION public.currenttimestamputc() RETURNS timestamp without time zone
    LANGUAGE sql STABLE
    AS $$ select current_timestamp at time zone 'UTC'; $$;


ALTER FUNCTION public.currenttimestamputc() OWNER TO dan;

--
-- Name: dailystatscaculate(); Type: FUNCTION; Schema: public; Owner: dan
--

CREATE FUNCTION public.dailystatscaculate() RETURNS bigint
    LANGUAGE plpgsql
    AS $$
	DECLARE
		queries		RECORD;
		curs1		REFCURSOR;
		TheDate		date;
		QueryTotal	bigint;

	BEGIN
		select (current_date - 1)::date
		INTO TheDate;

		FOR queries IN select id, query from daily_stats LOOP
			OPEN  curs1 FOR execute queries.query;
			FETCH curs1 INTO QueryTotal;
			EXECUTE 'INSERT INTO daily_stats_data (daily_stats_id, date, value) values (' 
					|| queries.id || ',' || quote_literal(TheDate) || ',' || quote_literal(QueryTotal) || ')';
			CLOSE curs1;
		END LOOP;
		return 0;
	END
$$;


ALTER FUNCTION public.dailystatscaculate() OWNER TO dan;

--
-- Name: dailysummarydateadd(date); Type: FUNCTION; Schema: public; Owner: dan
--

CREATE FUNCTION public.dailysummarydateadd(date) RETURNS bigint
    LANGUAGE plpgsql
    AS $_$
	DECLARE
	        DateToAdd	ALIAS for $1;

		RC		int8;

	BEGIN
		INSERT INTO daily_refreshes
		SELECT DateToAdd as refresh_date
		 WHERE NOT EXISTS (
		           SELECT refresh_date
		             FROM daily_refreshes
		            WHERE refresh_date = DateToAdd);

		GET DIAGNOSTICS RC = ROW_COUNT;

		RETURN RC;
	END
$_$;


ALTER FUNCTION public.dailysummarydateadd(date) OWNER TO dan;

--
-- Name: dailysummarydateremove(date); Type: FUNCTION; Schema: public; Owner: dan
--

CREATE FUNCTION public.dailysummarydateremove(date) RETURNS bigint
    LANGUAGE plpgsql
    AS $_$
	DECLARE
        DateToDelete	ALIAS for $1;

		RowCount    	int8;

	BEGIN
		DELETE FROM Daily_Refreshes
		 WHERE Refresh_Date = DateToDelete;

		GET DIAGNOSTICS RowCount = ROW_COUNT;

		RETURN RowCount;
	END
$_$;


ALTER FUNCTION public.dailysummarydateremove(date) OWNER TO dan;

--
-- Name: dearmor(text); Type: FUNCTION; Schema: public; Owner: dan
--

CREATE FUNCTION public.dearmor(text) RETURNS bytea
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/pgcrypto', 'pg_dearmor';


ALTER FUNCTION public.dearmor(text) OWNER TO dan;

--
-- Name: decrypt(bytea, bytea, text); Type: FUNCTION; Schema: public; Owner: dan
--

CREATE FUNCTION public.decrypt(bytea, bytea, text) RETURNS bytea
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/pgcrypto', 'pg_decrypt';


ALTER FUNCTION public.decrypt(bytea, bytea, text) OWNER TO dan;

--
-- Name: decrypt_iv(bytea, bytea, bytea, text); Type: FUNCTION; Schema: public; Owner: dan
--

CREATE FUNCTION public.decrypt_iv(bytea, bytea, bytea, text) RETURNS bytea
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/pgcrypto', 'pg_decrypt_iv';


ALTER FUNCTION public.decrypt_iv(bytea, bytea, bytea, text) OWNER TO dan;

--
-- Name: difference(text, text); Type: FUNCTION; Schema: public; Owner: dan
--

CREATE FUNCTION public.difference(text, text) RETURNS integer
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/fuzzystrmatch', 'difference';


ALTER FUNCTION public.difference(text, text) OWNER TO dan;

--
-- Name: digest(bytea, text); Type: FUNCTION; Schema: public; Owner: dan
--

CREATE FUNCTION public.digest(bytea, text) RETURNS bytea
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/pgcrypto', 'pg_digest';


ALTER FUNCTION public.digest(bytea, text) OWNER TO dan;

--
-- Name: digest(text, text); Type: FUNCTION; Schema: public; Owner: dan
--

CREATE FUNCTION public.digest(text, text) RETURNS bytea
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/pgcrypto', 'pg_digest';


ALTER FUNCTION public.digest(text, text) OWNER TO dan;

--
-- Name: dmetaphone(text); Type: FUNCTION; Schema: public; Owner: dan
--

CREATE FUNCTION public.dmetaphone(text) RETURNS text
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/fuzzystrmatch', 'dmetaphone';


ALTER FUNCTION public.dmetaphone(text) OWNER TO dan;

--
-- Name: dmetaphone_alt(text); Type: FUNCTION; Schema: public; Owner: dan
--

CREATE FUNCTION public.dmetaphone_alt(text) RETURNS text
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/fuzzystrmatch', 'dmetaphone_alt';


ALTER FUNCTION public.dmetaphone_alt(text) OWNER TO dan;

--
-- Name: element_add(text, character); Type: FUNCTION; Schema: public; Owner: dan
--

CREATE FUNCTION public.element_add(text, character) RETURNS integer
    LANGUAGE plpgsql
    AS $_$
   DECLARE 
      in_element_pathname         ALIAS for $1;
      element_directory_file_flag ALIAS for $2;
      Element_ID          int4;
      element_parent_id   int4;
      element_name        text;
      element_pathname    text;
      DebugText           text;
      position            int4;
      slash               text;

   BEGIN
      -- constants
      slash            := '/';

      -- put this into a local variable
      element_pathname := in_element_pathname;

      -- some debugging
      --DebugText := 'starting with "' || element_pathname || '"\\n';

      -- remove leading /, if any
      if substr(element_pathname, 1, 1) = slash then
         element_pathname := substr(element_pathname, 2);
      end if;

      -- start off with a null parent, as we are in the root directory
      element_parent_id = null;

      /* on error do something here */
      while(char_length(element_pathname) > 0) loop

         -- find the first / in the pathname
         position := strpos(element_pathname, slash);

         if position = 0 then
            -- no more slashes.  so grab the existing pathname as the element name
            element_name     := element_pathname;
            element_pathname := '';
         else
            -- adjust the pathname and element name accordingly
            element_name     := substr(element_pathname, 1, position-1);
            element_pathname := substr(element_pathname,    position+1);
         end if;

         -- get the id for this element.  how we do that depends
         -- on whether or not this element has a parent.

         if element_parent_id is null then
            --DebugText := DebugText || ' element id is null\\n';

            -- we have no parent_id yet.  so use the correct SQL
            select id 
              into element_id
              from element 
             where element.name = element_name 
               and parent_id    is null;
         else
            --DebugText := DebugText || ' element id is NOT null\\n';

            -- we have a parent_id. so use the correct SQL.
            select id 
              into element_id
              from element 
             where element.name = element_name 
               and parent_id    = element_parent_id;
         end if;


         if NOT FOUND then
            -- we did not find a parent for that element, so we insert that element
            --DebugText := DebugText || 'not found\\n';

            -- obtain the next number in the sequence
            element_id := nextval('element_id_seq');

            -- if we have exhausted the pathname, then we are inserting the final element
            -- otherwise, we are creating one of the final elements parents...
            -- set the directory/file flag accordingly.

             --DebugText := DebugText || element_id || ',';
             
             --DebugText := DebugText || element_directory_file_flag || ',' || element_name;
            if char_length(element_pathname) = 0 then
               insert into element(id, parent_id, directory_file_flag, name, status) 
                            values(element_id, element_parent_id, element_directory_file_flag, element_name, 'A');
            else
               insert into element(id, parent_id, directory_file_flag, name, status)
                            values(element_id, element_parent_id, 'D', element_name, 'A');
             end if;
         else
            --DebugText := DebugText || 'found\\n';
            -- we found a parent.
            if element_pathname = '' then
              -- element already exists
              Element_ID := 0;
            end if;
         end if;

        element_parent_id := element_id;
      end loop;

  return element_id;
end;
$_$;


ALTER FUNCTION public.element_add(text, character) OWNER TO dan;

--
-- Name: element_delete_check(); Type: FUNCTION; Schema: public; Owner: dan
--

CREATE FUNCTION public.element_delete_check() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
begin

   -- if we are removing an element, make sure we set the
   -- element status to deleted.

   if (NEW.change_type = 'R') then
--      select Element_SetStatus(new.element_id, 'D');
      update element
         set status = 'D'
       where id     = NEW.element_id;
   end if;

   --
   -- * * * * * * * * * * * * * * * *
   -- use a different return value if you change this
   -- function to a BEFORE
   -- * * * * * * * * * * * * * * * *
   RETURN NEW;
end;
$$;


ALTER FUNCTION public.element_delete_check() OWNER TO dan;

--
-- Name: element_id(text, text); Type: FUNCTION; Schema: public; Owner: dan
--

CREATE FUNCTION public.element_id(text, text) RETURNS integer
    LANGUAGE sql STABLE
    AS $_$
	SELECT E.id
	  FROM element E, ports P, categories C, ports_categories PC
	WHERE E.name         = $2
	  AND E.id           = P.element_id
	  AND C.name         = $1
	  AND PC.port_id     = P.id
	  AND PC.category_id = C.id
	  AND P.category_id  = C.id;
$_$;


ALTER FUNCTION public.element_id(text, text) OWNER TO dan;

--
-- Name: element_pathname(integer); Type: FUNCTION; Schema: public; Owner: dan
--

CREATE FUNCTION public.element_pathname(integer) RETURNS text
    LANGUAGE plpgsql IMMUTABLE
    AS $_$
   DECLARE
      element_id   ALIAS FOR $1;
      my_parent_id int4;
      element_name text;
      pathname     text;

begin
  pathname = '';
  select name, parent_id
    into element_name, my_parent_id 
    from element
   where id = element_id;

  IF FOUND THEN
    pathname := '/' || element_name || pathname;
    WHILE FOUND LOOP
      select name, parent_id 
        into element_name, my_parent_id
        from element
       where id = my_parent_id;

      IF FOUND THEN
        pathname = '/' || element_name || pathname;
      END IF;
    END LOOP;
  END IF;
  return pathname;
END;
$_$;


ALTER FUNCTION public.element_pathname(integer) OWNER TO dan;

--
-- Name: element_pathname(integer, boolean); Type: FUNCTION; Schema: public; Owner: dan
--

CREATE FUNCTION public.element_pathname(integer, boolean) RETURNS text
    LANGUAGE plpgsql STABLE
    AS $_$
   DECLARE
      element_id    ALIAS FOR $1;
      leading_slash ALIAS for $2;
      my_parent_id  int4;
      element_name  text;
      pathname      text;

begin
  pathname = '';
  select name, parent_id
    into element_name, my_parent_id
    from element
   where id = element_id;

  IF FOUND THEN
    pathname := '/' || element_name || pathname;
    WHILE FOUND LOOP
      select name, parent_id
        into element_name, my_parent_id
        from element
       where id = my_parent_id;

      IF FOUND THEN
         IF my_parent_id IS NULL THEN
           IF leading_slash THEN
              pathname = '/' || element_name || pathname;
           ELSE
              pathname = element_name || pathname;
           END IF;
           EXIT;
         ELSE
           pathname = '/' || element_name || pathname;
         END IF;
      END IF;
    END LOOP;
  END IF;

  return pathname;
END;
$_$;


ALTER FUNCTION public.element_pathname(integer, boolean) OWNER TO dan;

--
-- Name: element_pathname_insert(); Type: FUNCTION; Schema: public; Owner: dan
--

CREATE FUNCTION public.element_pathname_insert() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
   DECLARE
     l_pathname  text;
   BEGIN
      l_pathname := element_pathname(NEW.id);
      INSERT INTO element_pathname (element_id, pathname) VALUES (NEW.id, l_pathname);
      RETURN NEW;
   END
$$;


ALTER FUNCTION public.element_pathname_insert() OWNER TO dan;

--
-- Name: element_pathname_update(); Type: FUNCTION; Schema: public; Owner: dan
--

CREATE FUNCTION public.element_pathname_update() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
   BEGIN
		if (NEW.name != old.name OR new.parent_id != old.parent_id) THEN
			UPDATE element_pathname SET pathname = element_pathname(new.id) WHERE element_id = NEW.id;
                        PERFORM ElementPathnameUpdateChildren(new.id);
		END IF;
      RETURN NEW;
   END
$$;


ALTER FUNCTION public.element_pathname_update() OWNER TO dan;

--
-- Name: element_ports_status(); Type: FUNCTION; Schema: public; Owner: dan
--

CREATE FUNCTION public.element_ports_status() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
   BEGIN
      IF (NEW.status <> OLD.status) then
         UPDATE ports
            SET status = NEW.status
          WHERE ports.element_id = NEW.id;
      END IF;
      RETURN NEW;
   END
$$;


ALTER FUNCTION public.element_ports_status() OWNER TO dan;

--
-- Name: element_setstatus(integer, character); Type: FUNCTION; Schema: public; Owner: dan
--

CREATE FUNCTION public.element_setstatus(integer, character) RETURNS integer
    LANGUAGE plpgsql
    AS $_$
   DECLARE 
      element_id     ALIAS for $1;
      element_status ALIAS for $2;

   BEGIN
      update element 
         set status = element_status
       where id     = element_id;

     return element_id;
   END;
$_$;


ALTER FUNCTION public.element_setstatus(integer, character) OWNER TO dan;

--
-- Name: elementget(text); Type: FUNCTION; Schema: public; Owner: dan
--

CREATE FUNCTION public.elementget(text) RETURNS SETOF public.element_type
    LANGUAGE sql STABLE
    AS $_$
   SELECT id,
          name::text,
          directory_file_flag::text,
          status::text,
          case when IsCategory(Pathname_ID($1)) IS NULL THEN FALSE ELSE TRUE END,
          case when IsPort(    Pathname_ID($1)) IS NULL THEN FALSE ELSE TRUE END,
          element_pathname(id)
     FROM element
    WHERE id = PathName_ID($1);
$_$;


ALTER FUNCTION public.elementget(text) OWNER TO dan;

--
-- Name: elementpathnameupdatechildren(integer); Type: FUNCTION; Schema: public; Owner: dan
--

CREATE FUNCTION public.elementpathnameupdatechildren(integer) RETURNS integer
    LANGUAGE plpgsql
    AS $_$
  DECLARE
    p_ParentID  ALIAS FOR $1;

    l_element_pathname  RECORD;
    l_record_count      int4;
    l_temp_count        int4;

  BEGIN
    l_record_count := 0;
--    RAISE NOTICE 'ElementPathnameUpdateChildren invoked for: %', p_ParentID;
    FOR l_element_pathname IN SELECT * FROM element WHERE parent_id = p_ParentID LOOP
      l_record_count := l_record_count + 1;
--      RAISE NOTICE 'ElementPathnameUpdateChildren updating: %', l_element_pathname.id;
      UPDATE element_pathname 
         SET pathname   = element_pathname(l_element_pathname.id)
       WHERE element_id = l_element_pathname.id;

      IF l_element_pathname.directory_file_flag = 'D' THEN
        SELECT ElementPathnameUpdateChildren(l_element_pathname.id)
          INTO l_temp_count;
        l_record_count := l_record_count + l_temp_count;
      END IF;
    END LOOP;

    RETURN l_record_count;
  END;
$_$;


ALTER FUNCTION public.elementpathnameupdatechildren(integer) OWNER TO dan;

--
-- Name: elementrevisionid(integer, text); Type: FUNCTION; Schema: public; Owner: dan
--

CREATE FUNCTION public.elementrevisionid(integer, text) RETURNS integer
    LANGUAGE plpgsql
    AS $_$
   DECLARE 
      in_element_id ALIAS for $1;
      tag           ALIAS for $2;

      element_revision_id     int4;

   BEGIN
      select id
        into element_revision_id
        from element_revision
       where element_id    = in_element_id
         and revision_name = tag;

     return element_revision_id;
   END;
$_$;


ALTER FUNCTION public.elementrevisionid(integer, text) OWNER TO dan;

--
-- Name: elementtagset(integer, integer, text); Type: FUNCTION; Schema: public; Owner: dan
--

CREATE FUNCTION public.elementtagset(integer, integer, text) RETURNS boolean
    LANGUAGE plpgsql
    AS $_$
   DECLARE
      SystemBranchID  ALIAS for $1;
      ElementID       ALIAS for $2;
      RevisionName    ALIAS for $3;

      ExistingRevisionFound text;

   BEGIN  

-- has this element already been tagged with this version?
      ExistingRevisionFound := RevisionForTagGet(SystemBranchID, ElementID, RevisionName);

      IF ExistingRevisionFound IS NULL THEN
         insert into system_branch_element_revision
		(system_branch_id, element_id, revision_name)
	  values (SystemBranchID, ElementID, RevisionName);
      END IF;

      return 1;

   END;
$_$;


ALTER FUNCTION public.elementtagset(integer, integer, text) OWNER TO dan;

--
-- Name: encrypt(bytea, bytea, text); Type: FUNCTION; Schema: public; Owner: dan
--

CREATE FUNCTION public.encrypt(bytea, bytea, text) RETURNS bytea
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/pgcrypto', 'pg_encrypt';


ALTER FUNCTION public.encrypt(bytea, bytea, text) OWNER TO dan;

--
-- Name: encrypt_iv(bytea, bytea, bytea, text); Type: FUNCTION; Schema: public; Owner: dan
--

CREATE FUNCTION public.encrypt_iv(bytea, bytea, bytea, text) RETURNS bytea
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/pgcrypto', 'pg_encrypt_iv';


ALTER FUNCTION public.encrypt_iv(bytea, bytea, bytea, text) OWNER TO dan;

--
-- Name: freshports_branch_get(); Type: FUNCTION; Schema: public; Owner: dan
--

CREATE FUNCTION public.freshports_branch_get() RETURNS text
    LANGUAGE plpgsql
    AS $$

DECLARE
    reply TEXT;

BEGIN

   reply := current_setting('freshports.branch');

   IF reply IS NULL OR reply = '' THEN
      reply := 'head';
   END IF;

   RETURN reply;

END;

$$;


ALTER FUNCTION public.freshports_branch_get() OWNER TO dan;

--
-- Name: freshports_branch_set(text); Type: FUNCTION; Schema: public; Owner: dan
--

CREATE FUNCTION public.freshports_branch_set(text) RETURNS void
    LANGUAGE plpgsql
    AS $_$
    DECLARE
        BranchName ALIAS FOR $1;
    BEGIN

        PERFORM set_config('freshports.branch', BranchName, false);
        
    END;

$_$;


ALTER FUNCTION public.freshports_branch_set(text) OWNER TO dan;

--
-- Name: freshports_commit(text, integer, integer, integer); Type: FUNCTION; Schema: public; Owner: dan
--

CREATE FUNCTION public.freshports_commit(text, integer, integer, integer) RETURNS SETOF public.freshports_commit_record
    LANGUAGE plpgsql
    AS $_$
DECLARE
	p_MessageID   ALIAS FOR $1;
	p_Limit       ALIAS FOR $2;
	p_Offset      ALIAS FOR $3;
	p_UserID      ALIAS FOR $4;
	
	l_CommitLogID	int8;
	r               freshports_commit_record%rowtype;
BEGIN

	-- is this a port commit or not?	
	SELECT CLP.commit_log_id
	  INTO l_CommitLogID
	  FROM commit_log CL, commit_log_ports CLP
	 WHERE CL.message_id = p_MessageID
	   AND CL.id         = CLP.commit_log_id;

	IF FOUND THEN
		FOR r IN
			SELECT * FROM freshports_commit_port(p_MessageID, p_Limit, p_Offset, p_UserID)
		LOOP
			RETURN NEXT r;
		END LOOP;
	ELSE
		FOR r IN
			SELECT * FROM freshports_commit_regular(p_MessageID, p_Limit, p_Offset, p_UserID)
		LOOP
			RETURN NEXT r;
		END LOOP;
	END IF;

	RETURN;
END
$_$;


ALTER FUNCTION public.freshports_commit(text, integer, integer, integer) OWNER TO dan;

--
-- Name: freshports_commit_count_elements(text); Type: FUNCTION; Schema: public; Owner: dan
--

CREATE FUNCTION public.freshports_commit_count_elements(text) RETURNS bigint
    LANGUAGE sql STABLE
    AS $_$
 SELECT count(*)
   FROM commit_log CL JOIN commit_log_ports_elements CLPE ON CL.id = CLPE.commit_log_id
  WHERE CL.message_id = $1;
$_$;


ALTER FUNCTION public.freshports_commit_count_elements(text) OWNER TO dan;

--
-- Name: freshports_commit_port(text, integer, integer, integer); Type: FUNCTION; Schema: public; Owner: dan
--

CREATE FUNCTION public.freshports_commit_port(text, integer, integer, integer) RETURNS SETOF public.freshports_commit_record
    LANGUAGE sql STABLE
    AS $_$
SELECT FOURTH1.port_id,
       FOURTH1.short_description,
       FOURTH1.date_added,
       FOURTH1.needs_refresh,
       C.name   AS category,
       C.id     AS category_id,
       FOURTH1.message_id,
       FOURTH1.committer   AS committer,
       FOURTH1.commit_description AS commit_description,
       FOURTH1.commit_log_id,
       CASE WHEN FOURTH1.port_id IS NULL THEN NULL ELSE FOURTH1.name END AS port,
       to_char(FOURTH1.commit_date - SystemTimeAdjust(), 'DD Mon YYYY')  AS commit_date,
       FOURTH1.commit_date - SystemTimeAdjust()                          AS commit_date_raw,
       to_char(FOURTH1.commit_date - SystemTimeAdjust(), 'HH24:MI:SS')   AS commit_time,
       FOURTH1.encoding_losses,
       FOURTH1.status     AS status,
       FOURTH1.port_version,
       FOURTH1.port_revision,
       FOURTH1.port_epoch,
       NULL::text        AS revision,
       NULL::text         AS change_type,
       FOURTH1.pathname   AS pathname,
       CASE WHEN FOURTH1.port_id IS NULL THEN FALSE ELSE TRUE END AS is_port,
       FOURTH1.element_id,
       FOURTH1.forbidden,
       FOURTH1.deprecated,
       FOURTH1.ignore,
       FOURTH1.broken,
       FOURTH1.vulnerable_current,
       FOURTH1.vulnerable_past,
       FOURTH1.restricted,
       FOURTH1.expiration_date,
       FOURTH1.no_cdrom,
       FOURTH1.is_interactive,
       FOURTH1.only_for_archs,
       FOURTH1.not_for_archs,
       FOURTH1.svn_revision,
       FOURTH1.svn_hostname,
       FOURTH1.path_to_repo,
       FOURTH1.repo_name,
       OnWatchList($4, FOURTH1.element_id) as watch
FROM
(SELECT THIRD1b.*,
        ports_vulnerable.current AS vulnerable_current,
        ports_vulnerable.past    AS vulnerable_past
FROM
(
SELECT THIRD1.*,
       CLP.port_version,
       CLP.port_revision,
       CLP.port_epoch,
       CLP.needs_refresh
FROM 
(SELECT SECOND1.*,
       element_pathname(E.id, false)   AS pathname,
       CASE WHEN port_id IS NULL THEN '' ELSE E.name END                  as name,
       E.status                 AS status
FROM
(SELECT FIRST1.*,
        P.id as port_id,
        P.date_added,
        P.short_description,
        P.category_id,
        P.forbidden,
        P.deprecated,
        P.ignore,
        P.broken,
        P.restricted,
        P.expiration_date,
        P.no_cdrom,
        P.is_interactive,
        P.only_for_archs,
        P.not_for_archs
FROM         
(SELECT CL.message_id   AS message_id,
        CL.committer    AS committer,
        CL.description  AS commit_description,
        CL.id           AS commit_log_id,
        CLPE.element_id AS element_id,
        CL.commit_date,
        CL.encoding_losses,
	CL.svn_revision,
	R.svn_hostname,
	R.path_to_repo,
	R.name as repo_name
   FROM commit_log CL LEFT OUTER JOIN repo R ON CL.repo_id = R.id 
JOIN commit_log_ports_elements CLPE ON CL.id = CLPE.commit_log_id
  WHERE CL.message_id = $1) AS FIRST1
       LEFT OUTER JOIN ports P ON P.element_id = FIRST1.element_id) AS SECOND1
       JOIN element E on SECOND1.element_id = E.id) AS THIRD1
       LEFT OUTER JOIN commit_log_ports CLP ON THIRD1.port_id = CLP.port_ID AND THIRD1.commit_log_id = CLP.commit_log_id) AS THIRD1b
   LEFT OUTER JOIN ports_vulnerable ON THIRD1b.port_id = ports_vulnerable.port_ID) AS FOURTH1
       LEFT OUTER JOIN categories C ON FOURTH1.category_id = C.id
order by port, pathname
   LIMIT $2
  OFFSET $3;

$_$;


ALTER FUNCTION public.freshports_commit_port(text, integer, integer, integer) OWNER TO dan;

--
-- Name: freshports_commit_regular(text, integer, integer, integer); Type: FUNCTION; Schema: public; Owner: dan
--

CREATE FUNCTION public.freshports_commit_regular(text, integer, integer, integer) RETURNS SETOF public.freshports_commit_record
    LANGUAGE sql STABLE
    AS $_$
SELECT 0 AS port_id,
       NULL::text AS short_description,
       NULL::timestamp with time zone AS date_added,
       NULL::smallint AS needs_refresh,
       NULL::text AS category,
       NULL::integer AS category_id,
       CL.message_id,
       CL.committer   AS committer,
       CL.description AS commit_description,
       CL.id AS commit_log_id,
       NULL::text AS port,
       to_char(CL.commit_date - SystemTimeAdjust(), 'DD Mon YYYY')  AS commit_date,
       CL.commit_date - SystemTimeAdjust()                          AS commit_date_raw,
       to_char(CL.commit_date - SystemTimeAdjust(), 'HH24:MI:SS')   AS commit_time,
       CL.encoding_losses,
       E.status   AS status,
       NULL::text AS port_version,
       NULL::text AS port_revision,
       NULL::text AS port_epoch,
       CLE.revision_name        AS revision,
       CLE.change_type::text    AS change_type,
       element_pathname(E.id)   AS element_pathname,
       FALSE AS is_port,
       CLE.element_id,
       NULL::text AS forbidden,
       NULL::text AS deprecated,
       NULL::text AS ignore,
       NULL::text AS broken,
       NULL::int  AS vulnerable_current,
       NULL::int  AS vulnerable_past,
       NULL::text AS restricted,
       NULL::date AS expiration_date,
       NULL::text AS no_cdrom,
       NULL::text AS is_interactive,
       NULL::text AS only_for_archs,
       NULL::text AS not_for_archs,
       CL.svn_revision,
       R.svn_hostname,
       R.path_to_repo,
       R.name AS repo_name,
       OnWatchList($4, CLE.element_id) AS watch
  FROM commit_log CL LEFT OUTER JOIN repo R ON CL.repo_id = R.id LEFT OUTER JOIN commit_log_elements CLE ON (CL.id = CLE.commit_log_id)
       LEFT OUTER JOIN element E ON (CLE.element_id = E.id)
 WHERE CL.message_id  = $1
ORDER BY port, element_pathname
   LIMIT $2
  OFFSET $3;
  
  -- NOTE some commits touch nothing....  e.g. 201109230051.p8N0pbV2045995@svn.freebsd.org

$_$;


ALTER FUNCTION public.freshports_commit_regular(text, integer, integer, integer) OWNER TO dan;

--
-- Name: gen_random_bytes(integer); Type: FUNCTION; Schema: public; Owner: dan
--

CREATE FUNCTION public.gen_random_bytes(integer) RETURNS bytea
    LANGUAGE c STRICT
    AS '$libdir/pgcrypto', 'pg_random_bytes';


ALTER FUNCTION public.gen_random_bytes(integer) OWNER TO dan;

--
-- Name: gen_salt(text); Type: FUNCTION; Schema: public; Owner: dan
--

CREATE FUNCTION public.gen_salt(text) RETURNS text
    LANGUAGE c STRICT
    AS '$libdir/pgcrypto', 'pg_gen_salt';


ALTER FUNCTION public.gen_salt(text) OWNER TO dan;

--
-- Name: gen_salt(text, integer); Type: FUNCTION; Schema: public; Owner: dan
--

CREATE FUNCTION public.gen_salt(text, integer) RETURNS text
    LANGUAGE c STRICT
    AS '$libdir/pgcrypto', 'pg_gen_salt_rounds';


ALTER FUNCTION public.gen_salt(text, integer) OWNER TO dan;

--
-- Name: generate_watch_list_token(); Type: FUNCTION; Schema: public; Owner: dan
--

CREATE FUNCTION public.generate_watch_list_token() RETURNS text
    LANGUAGE sql
    AS $$
    SELECT md5((random())::text) || md5((random())::text) as token;
$$;


ALTER FUNCTION public.generate_watch_list_token() OWNER TO dan;

--
-- Name: getcategory(integer); Type: FUNCTION; Schema: public; Owner: dan
--

CREATE FUNCTION public.getcategory(integer) RETURNS text
    LANGUAGE plpgsql
    AS $_$
	DECLARE
		category_id			ALIAS for $1;

		category_name		text;

	BEGIN
		select name
		  into category_name
		  from categories
		 where id = category_id;

		return category_name;
	END;
$_$;


ALTER FUNCTION public.getcategory(integer) OWNER TO dan;

--
-- Name: getcategory(text); Type: FUNCTION; Schema: public; Owner: dan
--

CREATE FUNCTION public.getcategory(text) RETURNS integer
    LANGUAGE plpgsql
    AS $_$
   DECLARE
      category_name ALIAS for $1;     
      pathname             text;   
      category_element_id  int4;
      category_id          int4;
   BEGIN
      pathname            := 'ports/head/' || category_name;
      category_element_id := Pathname_ID(pathname);
      if NOT category_element_id IS NULL THEN
         select id
           into category_id
           from categories
          where element_id = category_element_id;
      END IF;
      return category_id;    
   END;
$_$;


ALTER FUNCTION public.getcategory(text) OWNER TO dan;

--
-- Name: getcategoryportfromlatestlink(text); Type: FUNCTION; Schema: public; Owner: dan
--

CREATE FUNCTION public.getcategoryportfromlatestlink(text) RETURNS text
    LANGUAGE plpgsql STABLE
    AS $_$
--
-- This function takes a package name, as taken from LATEST_LINK
-- and returns the CATEGORY/PORT for that package.
-- It returns "0" if nothing found.
-- It returns "-1" if more than one match found. 
--
   DECLARE
      p_PackageName ALIAS FOR $1;

      l_Count       int;
      l_Category    TEXT;
      l_Port        TEXT;
      l_Result      TEXT;

   BEGIN
      SELECT count(*)
        INTO l_Count
        FROM ports_active
       WHERE package_name = p_PackageName;

      IF l_Count = 1 THEN
         SELECT category,   name
           INTO l_Category, l_Port
           FROM ports_active
          WHERE package_name = p_PackageName;

         l_Result := l_Category || '/' || l_Port;

      ELSE
         IF l_Count = 0 THEN
            l_Result := '0';
         ELSE
            IF l_Count > 1 THEN
               l_Result := '-1';
            END IF;
        END IF;
      END IF;

      return l_Result;

   END;
$_$;


ALTER FUNCTION public.getcategoryportfromlatestlink(text) OWNER TO dan;

--
-- Name: getparentid(integer); Type: FUNCTION; Schema: public; Owner: dan
--

CREATE FUNCTION public.getparentid(integer) RETURNS integer
    LANGUAGE sql STABLE
    AS $_$
	  SELECT parent_id 
	    FROM element
	   WHERE id = $1
$_$;


ALTER FUNCTION public.getparentid(integer) OWNER TO dan;

--
-- Name: getport(text); Type: FUNCTION; Schema: public; Owner: dan
--

CREATE FUNCTION public.getport(text) RETURNS integer
    LANGUAGE plpgsql
    AS $_$
   DECLARE
      category_port ALIAS for $1;  
      pathname         text;
      port_element_id  int4;       
      port_id          int4;
      l_branch         text;
   BEGIN
      SELECT freshports_branch_get()
        INTO l_branch;

--      RAISE NOTICE ' branch is %', l_branch;

      IF l_branch = 'head' THEN
          pathname := '/ports/head/'                        || category_port;
      ELSE
          pathname := '/ports/branches/' || l_branch || '/' || category_port;
      END IF;

--      RAISE NOTICE ' GetPort pathname is=%', pathname;

      port_element_id := Pathname_ID(pathname);
      IF port_element_id IS NOT NULL THEN
         SELECT id
           INTO port_id
           FROM ports    
          WHERE element_id = port_element_id;    
      END IF;

      RETURN port_id;
   END;
$_$;


ALTER FUNCTION public.getport(text) OWNER TO dan;

--
-- Name: getport(text, text); Type: FUNCTION; Schema: public; Owner: dan
--

CREATE FUNCTION public.getport(text, text) RETURNS integer
    LANGUAGE plpgsql STABLE
    AS $_$
   DECLARE
      p_Category ALIAS for $1;  
      p_Port     ALIAS for $2;

      l_port_id          int4;
   BEGIN
      RAISE WARNING 'GetPort has been called with %/% - this procedure is not branch aware.', p_Category, p_Port;
      SELECT id
        INTO l_port_id
        FROM ports_active
       WHERE name     = p_Port
         AND category = p_Category;

      RETURN l_port_id;
   END;
$_$;


ALTER FUNCTION public.getport(text, text) OWNER TO dan;

--
-- Name: getportfrompackagename(text); Type: FUNCTION; Schema: public; Owner: dan
--

CREATE FUNCTION public.getportfrompackagename(text) RETURNS SETOF public.getportfrompackagename_record
    LANGUAGE plpgsql
    AS $_$
DECLARE
	a_PackageName	ALIAS for $1;

	r		GetPortFromPackageName_RECORD%rowtype;

BEGIN
	FOR r IN
		SELECT P.id AS port_id, C.name AS category, packagename(P.id) as port, element_pathname(P.element_id)
		  FROM ports P JOIN element_pathname EP ON P.element_id  = EP.element_id
		               JOIN categories       C  ON P.category_id = C.id
		 WHERE package_name = a_PackageName
		   AND EP.pathname LIKE '/ports/head/%'

	LOOP
		RETURN NEXT r;
	END LOOP;
END $_$;


ALTER FUNCTION public.getportfrompackagename(text) OWNER TO dan;

--
-- Name: getportid(text, text); Type: FUNCTION; Schema: public; Owner: dan
--

CREATE FUNCTION public.getportid(text, text) RETURNS bigint
    LANGUAGE plpgsql
    AS $_$
   DECLARE
      CategoryName ALIAS for $1;
      PortName     ALIAS for $2;

      CategoryID   int8;
      IsPrimary    boolean;
      PortID       int8;

   BEGIN

      RAISE WARNING 'GetPort has been called with %/% - this procedure is not branch aware.', CategoryName, PortName;
      SELECT id, is_primary
        INTO CategoryID, IsPrimary
        FROM categories
       WHERE name = CategoryName;

      IF FOUND THEN
         IF IsPrimary THEN
            SELECT ports_all.id
              INTO PortID
              FROM ports_all, categories
             WHERE ports_all.category_id = categories.id
               AND ports_all.name        = PortName
               AND categories.id         = CategoryID;
         ELSE
            SELECT ports_all.id
              INTO PortID
              FROM ports_categories, ports_all
             WHERE ports_categories.port_id     = ports_all.id
               AND ports_categories.category_id = CategoryID
               AND ports_all.name               = PortName;
         END IF;
      ELSE
      END IF;

      return PortID;

   END
$_$;


ALTER FUNCTION public.getportid(text, text) OWNER TO dan;

--
-- Name: getportnamefromfilename(text); Type: FUNCTION; Schema: public; Owner: dan
--

CREATE FUNCTION public.getportnamefromfilename(text) RETURNS text
    LANGUAGE plpgsql
    AS $_$
	DECLARE
 		file_name	ALIAS for $1;

		temp		text;
        port		text;
		category	text;

		pos			int4;

    BEGIN
		RAISE WARNING 'GetPortNameFromFileName is not branch aware';
		-- remove the first /
		temp := substring(file_name from 2);

		-- remove everything before the 2nd /
		pos := position('/' in temp);

		category := substring(temp from 1 for pos - 1);

		temp := substring(temp from pos + 1);

		port := substring(temp from 1 for pos - 1);

        return category || '/' || port;
    END;
$_$;


ALTER FUNCTION public.getportnamefromfilename(text) OWNER TO dan;

--
-- Name: glob_to_regex(text); Type: FUNCTION; Schema: public; Owner: dan
--

CREATE FUNCTION public.glob_to_regex(text) RETURNS text
    LANGUAGE plperl
    AS $_$
    my $w = quotemeta shift;
    $w =~ s{\\([*?]|\\(\\?.)|\[(?:\\\!)?(?:\\\])?[^]]*\\\])}
                   {($1 eq '*' ? '.*' :
                         $1 eq '?' ? '.'  :
                         defined($2) ? $2 :
                         do { my $p = $1; $p =~ s/\\([][^-])/$1/g; $p =~ s/^\[\\!/[^/; $p; })}eg;
    return $w;
$_$;


ALTER FUNCTION public.glob_to_regex(text) OWNER TO dan;

--
-- Name: gmt_format(timestamp with time zone); Type: FUNCTION; Schema: public; Owner: dan
--

CREATE FUNCTION public.gmt_format(timestamp with time zone) RETURNS text
    LANGUAGE sql IMMUTABLE
    AS $_$
	SELECT to_char($1 AT TIME ZONE 'UTC', 'Dy, DD Mon YYYY HH24:MI:SS') || ' GMT' as GMT;
$_$;


ALTER FUNCTION public.gmt_format(timestamp with time zone) OWNER TO dan;

--
-- Name: hmac(bytea, bytea, text); Type: FUNCTION; Schema: public; Owner: dan
--

CREATE FUNCTION public.hmac(bytea, bytea, text) RETURNS bytea
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/pgcrypto', 'pg_hmac';


ALTER FUNCTION public.hmac(bytea, bytea, text) OWNER TO dan;

--
-- Name: hmac(text, text, text); Type: FUNCTION; Schema: public; Owner: dan
--

CREATE FUNCTION public.hmac(text, text, text) RETURNS bytea
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/pgcrypto', 'pg_hmac';


ALTER FUNCTION public.hmac(text, text, text) OWNER TO dan;

--
-- Name: id_change(); Type: FUNCTION; Schema: public; Owner: dan
--

CREATE FUNCTION public.id_change() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
begin
   --  message 'check_siblings 4';
   if (new.id <> old.id) then
      -- message 'change of id not allowed ' || NOW(*);
      RAISE EXCEPTION 'modifications to element % with name=% cannot be completed as you are not allowed to change id (new value was %).', old.id, old.name, new.id;
   end if;

   RETURN NEW;
end;
$$;


ALTER FUNCTION public.id_change() OWNER TO dan;

--
-- Name: iscategory(integer); Type: FUNCTION; Schema: public; Owner: dan
--

CREATE FUNCTION public.iscategory(integer) RETURNS integer
    LANGUAGE sql STABLE
    AS $_$
   SELECT id
     FROM categories
    WHERE element_id = $1;
$_$;


ALTER FUNCTION public.iscategory(integer) OWNER TO dan;

--
-- Name: isdescendantof(integer, integer); Type: FUNCTION; Schema: public; Owner: dan
--

CREATE FUNCTION public.isdescendantof(integer, integer) RETURNS boolean
    LANGUAGE plpgsql
    AS $_$
   DECLARE
      element_id ALIAS FOR $1;
      target_id  ALIAS FOR $2;

      my_parent_id   int4;
      IsDescendantOf boolean;
      looping        int4;

   BEGIN
      IsDescendantOf := 0;
      IF element_id <> target_id THEN
         my_parent_id := element_id;
         looping := 1;
         WHILE looping=1 LOOP

            select parent_id into my_parent_id
              from element
             where id = my_parent_id;

            IF FOUND THEN
               IF my_parent_id = target_id THEN
                  IsDescendantOf := 1;
                  looping        := 0;
               END IF;
            ELSE
               looping := 0;
            END IF;
         END LOOP;
      END IF;

      RETURN IsDescendantOf;
   END;
$_$;


ALTER FUNCTION public.isdescendantof(integer, integer) OWNER TO dan;

--
-- Name: isloginvalid(text, text); Type: FUNCTION; Schema: public; Owner: dan
--

CREATE FUNCTION public.isloginvalid(text, text) RETURNS integer
    LANGUAGE plpgsql
    AS $_$
   DECLARE
      UserName     ALIAS for $1;
      UserPassWord ALIAS for $2;
      UserID       int4;

   BEGIN
      select id
        into UserID
        from Users
       where lower(name)   = lower(UserName)
         and password_hash = UserPassWord;

      return UserID;
   END;
$_$;


ALTER FUNCTION public.isloginvalid(text, text) OWNER TO dan;

--
-- Name: isport(integer); Type: FUNCTION; Schema: public; Owner: dan
--

CREATE FUNCTION public.isport(integer) RETURNS integer
    LANGUAGE sql STABLE
    AS $_$
   SELECT id
     FROM ports
    WHERE element_id = $1;
$_$;


ALTER FUNCTION public.isport(integer) OWNER TO dan;

--
-- Name: isportstree(text); Type: FUNCTION; Schema: public; Owner: dan
--

CREATE FUNCTION public.isportstree(text) RETURNS boolean
    LANGUAGE plpgsql
    AS $_$

--
-- return TRUE if the element exists under the ports tree
---
   DECLARE
      element_pathname ALIAS for $1;

      IsPortsTree   boolean;
   BEGIN
      IsPortsTree := 0;
      if (substring(element_pathname from 1 for 6) = '/ports') OR
         (substring(element_pathname from 1 for 5) =  'ports')  THEN
          if Pathname_ID(element_pathname) then
             IsPortsTree := 1;
          END IF;
      END IF;

      RETURN IsPortsTree;
   END;
$_$;


ALTER FUNCTION public.isportstree(text) OWNER TO dan;

--
-- Name: latest_commits_anchor(); Type: FUNCTION; Schema: public; Owner: dan
--

CREATE FUNCTION public.latest_commits_anchor() RETURNS integer
    LANGUAGE sql STABLE
    AS $$
  SELECT commit_log_id AS RESULT
    FROM latest_commits
ORDER BY commit_log_id
   LIMIT 1;
$$;


ALTER FUNCTION public.latest_commits_anchor() OWNER TO dan;

--
-- Name: latest_commits_ports_anchor(); Type: FUNCTION; Schema: public; Owner: dan
--

CREATE FUNCTION public.latest_commits_ports_anchor() RETURNS integer
    LANGUAGE sql STABLE
    AS $$
  SELECT commit_log_id AS RESULT
    FROM latest_commits_ports
ORDER BY commit_log_id
   LIMIT 1;
$$;


ALTER FUNCTION public.latest_commits_ports_anchor() OWNER TO dan;

--
-- Name: latestcommitdate(); Type: FUNCTION; Schema: public; Owner: dan
--

CREATE FUNCTION public.latestcommitdate() RETURNS timestamp with time zone
    LANGUAGE sql STABLE
    AS $$
   SELECT CL.date_added
     FROM commit_log CL
 ORDER BY CL.id desc limit 1;
$$;


ALTER FUNCTION public.latestcommitdate() OWNER TO dan;

--
-- Name: latestcommitdateports(); Type: FUNCTION; Schema: public; Owner: dan
--

CREATE FUNCTION public.latestcommitdateports() RETURNS timestamp with time zone
    LANGUAGE sql STABLE
    AS $$
SELECT CL.commit_date as last_commit_date
  FROM commit_log_ports CLP
  JOIN commit_log       CL on CL.id = CLP.commit_log_id
 ORDER BY CL.commit_date DESC
  LIMIT 1
$$;


ALTER FUNCTION public.latestcommitdateports() OWNER TO dan;

--
-- Name: latestcommits(integer, integer, text); Type: FUNCTION; Schema: public; Owner: dan
--

CREATE FUNCTION public.latestcommits(integer, integer, text) RETURNS SETOF public.commit_record
    LANGUAGE plpgsql
    AS $_$
DECLARE
	MaxCommits	ALIAS for $1;
	UserID		ALIAS for $2;
    Branch      ALIAS for $3;

	r 	commit_record%rowtype;

BEGIN
	IF MaxCommits = 100 THEN
		FOR r IN
			SELECT * FROM LatestCommitsLarge(UserID, Branch)
		LOOP
    	    RETURN NEXT r;
	    END LOOP;
	ELSE
		FOR r IN
			SELECT * FROM LatestCommitsSmall(UserID, Branch)
		LOOP
    	    RETURN NEXT r;
	    END LOOP;
	END IF;

    RETURN;
END
$_$;


ALTER FUNCTION public.latestcommits(integer, integer, text) OWNER TO dan;

--
-- Name: latestcommitsfiltered(integer, integer, text); Type: FUNCTION; Schema: public; Owner: dan
--

CREATE FUNCTION public.latestcommitsfiltered(integer, integer, text) RETURNS SETOF public.commit_record
    LANGUAGE plpgsql
    AS $_$
DECLARE
	MaxCommits	ALIAS for $1;
	UserID		ALIAS for $2;
	Filter		ALIAS for $3;

	r 	commit_record%rowtype;

BEGIN
	IF MaxCommits = 100 THEN
		FOR r IN
			SELECT * FROM LatestCommitsLargeFiltered(UserID, Filter)
		LOOP
    	    RETURN NEXT r;
	    END LOOP;
	ELSE
		FOR r IN
			SELECT * FROM LatestCommitsSmall(UserID)
		LOOP
    	    RETURN NEXT r;
	    END LOOP;
	END IF;

    RETURN;
END
$_$;


ALTER FUNCTION public.latestcommitsfiltered(integer, integer, text) OWNER TO dan;

--
-- Name: latestcommitslarge(integer, text); Type: FUNCTION; Schema: public; Owner: dan
--

CREATE FUNCTION public.latestcommitslarge(integer, text) RETURNS SETOF public.commit_record
    LANGUAGE plpgsql
    AS $_$
DECLARE
	UserID		ALIAS for $1;
    Branch      ALIAS for $2;

	r 	commit_record%rowtype;

BEGIN
    FOR r IN
SELECT SECURITY.commit_log_id,
       SECURITY.commit_date_raw,
       SECURITY.message_subject,
       SECURITY.message_id,
       SECURITY.committer,
       SECURITY.commit_description,
       SECURITY.commit_date,
       SECURITY.commit_time,
       SECURITY.encoding_losses,
       SECURITY.port_id,
       SECURITY.needs_refresh,
       SECURITY.forbidden,
       SECURITY.broken,
       SECURITY.deprecated,
       SECURITY.ignore,
       SECURITY.element_id,
       SECURITY.version,
       SECURITY.revision,
       SECURITY.epoch,
       SECURITY.date_added,
       SECURITY.short_description,
       SECURITY.category_id,
       SECURITY.port,
       SECURITY.status,
       SECURITY.category,
       SECURITY.vulnerable_current,
       SECURITY.vulnerable_past,
       SECURITY.restricted,
       SECURITY.no_cdrom,
       SECURITY.expiration_date,
       SECURITY.is_interactive,
       SECURITY.only_for_archs,
       SECURITY.not_for_archs,
       SECURITY.repo_name,
       SECURITY.svn_revision,
       SECURITY.svn_hostname,
       SECURITY.path_to_repo,
       W.watch,
       element_pathname(SECURITY.element_id, FALSE) as element_pathname
FROM (
SELECT PEC.*
FROM (
SELECT PORTELEMENT.*,
       categories.name AS category
FROM (
SELECT LCPPORTS.*,
       element.name    AS port,
       element.status  AS status,
        CASE WHEN port_id IS NULL THEN commit_log_id ELSE NULL END AS non_port_commit_log_id,
        CASE when clp_version  IS NULL then ports_version  else clp_version  END as version,
        CASE when clp_revision IS NULL then ports_revision else clp_revision END AS revision,
        CASE when clp_epoch    IS NULL then ports_epoch    else clp_epoch    END AS epoch
FROM (
  SELECT VERSIONS_VULN.*, 
        CLP.port_version  AS clp_version,
        CLP.port_revision AS clp_revision,
        CLP.port_epoch    AS clp_epoch,
        CLP.needs_refresh AS needs_refresh
FROM
(
SELECT VERSIONS.*,
       ports_vulnerable.current AS vulnerable_current,
       ports_vulnerable.past    AS vulnerable_past
FROM
( 
SELECT CLPE.*,
         P.id as port_id,
         P.forbidden,
         P.broken,
         P.deprecated,
         P.ignore,
         date_part('epoch', P.date_added)::integer AS date_added,
         P.short_description              AS short_description,
         P.category_id,
         P.revision  as ports_revision,
         P.version   as ports_version,
         P.portepoch as ports_epoch,
         P.restricted,
         P.expiration_date,
         P.no_cdrom,
         P.is_interactive,
         P.only_for_archs,
         P.not_for_archs
    FROM


  (SELECT LCPCL.*,
          CLPE.element_id,
          element_pathname(CLPE.element_id)
   FROM 
   (SELECT CL.id       AS commit_log_id, 
           commit_date AS commit_date_raw,
           message_subject,
           message_id,
           committer,
           CL.description AS commit_description,
           to_char(CL.commit_date - SystemTimeAdjust(), 'DD Mon YYYY')  AS commit_date,
           to_char(CL.commit_date - SystemTimeAdjust(), 'HH24:MI')      AS commit_time,
           encoding_losses,
           svn_revision,
           repo.name AS repo_name,
           repo.svn_hostname,
           repo.path_to_repo
     FROM commit_log CL LEFT OUTER JOIN repo ON CL.repo_id = repo.id JOIN 
                (SELECT LCP.commit_log_id
                   FROM latest_commits_ports LCP JOIN commit_log_branches CLB ON LCP.commit_log_id = CLB.commit_log_id
                            JOIN system_branch SB ON SB.branch_name = Branch AND SB.id = CLB.branch_id
               ORDER BY LCP.commit_date DESC
                  LIMIT 10) AS 
                  LCP ON CL.id = LCP.commit_log_id) AS LCPCL
                      JOIN commit_log_ports_elements CLPE on CLPE.commit_log_id = LCPCL.commit_log_id
                                                         AND CLPE.commit_log_id > latest_commits_ports_anchor()


) AS CLPE
           LEFT OUTER JOIN ports P ON CLPE.element_id = P.element_id) AS VERSIONS
           LEFT OUTER JOIN ports_vulnerable ON VERSIONS.port_id = ports_vulnerable.port_ID) AS VERSIONS_VULN
           LEFT OUTER JOIN commit_log_ports CLP ON CLP.commit_log_id = VERSIONS_VULN.commit_log_id
                                     AND VERSIONS_VULN.port_id = CLP.port_id
                                AND CLP.commit_log_id > latest_commits_ports_anchor()


) AS LCPPORTS JOIN element
on LCPPORTS.element_id = element.id) AS PORTELEMENT LEFT JOIN categories
on PORTELEMENT.category_id = categories.id)

 AS PEC) AS SECURITY LEFT OUTER JOIN
(SELECT element_id as wle_element_id, COUNT(watch_list_id) as watch
  FROM watch_list JOIN watch_list_element
        ON watch_list.id      = watch_list_element.watch_list_id
       AND watch_list.user_id = UserID
       AND watch_list.in_service
GROUP BY wle_element_id) AS W ON W.wle_element_id = SECURITY.element_id
order by commit_date_raw desc, category, port

	LOOP
        RETURN NEXT r;
    END LOOP;
    RETURN;
END
$_$;


ALTER FUNCTION public.latestcommitslarge(integer, text) OWNER TO dan;

--
-- Name: latestcommitslargefiltered(integer, text); Type: FUNCTION; Schema: public; Owner: dan
--

CREATE FUNCTION public.latestcommitslargefiltered(integer, text) RETURNS SETOF public.commit_record
    LANGUAGE plpgsql
    AS $_$
DECLARE
	UserID		ALIAS for $1;
	Filter		ALIAS for $2;

	r 	commit_record%rowtype;

BEGIN
    FOR r IN
SELECT SECURITY.commit_log_id,
       SECURITY.commit_date_raw,
       SECURITY.message_subject,
       SECURITY.message_id,
       SECURITY.committer,
       SECURITY.commit_description,
       SECURITY.commit_date,
       SECURITY.commit_time,
       SECURITY.encoding_losses,
       SECURITY.port_id,
       SECURITY.needs_refresh,
       SECURITY.forbidden,
       SECURITY.broken,
       SECURITY.deprecated,
       SECURITY.ignore,
       SECURITY.element_id,
       SECURITY.version,
       SECURITY.revision,
       SECURITY.epoch,
       SECURITY.date_added,
       SECURITY.short_description,
       SECURITY.category_id,
       SECURITY.port,
       SECURITY.status,
       SECURITY.category,
       SECURITY.vulnerable_current,
       SECURITY.vulnerable_past,
       SECURITY.restricted,
       SECURITY.no_cdrom,
       SECURITY.expiration_date,
       SECURITY.is_interactive,
       SECURITY.only_for_archs,
       SECURITY.not_for_archs,
       SECURITY.svn_revision,
       SECURITY.svn_hostname,
       SECURITY.path_to_repo,
       W.watch,
       element_pathname(SECURITY.element_id, FALSE) as element_pathname
FROM (
SELECT PEC.*
FROM (
SELECT PORTELEMENT.*,
       categories.name AS category
FROM (
SELECT LCPPORTS.*,
       element.name    AS port,
       element.status  AS status,
        CASE WHEN port_id IS NULL THEN commit_log_id ELSE NULL END AS non_port_commit_log_id,
        CASE when clp_version  IS NULL then ports_version  else clp_version  END as version,
        CASE when clp_revision IS NULL then ports_revision else clp_revision END AS revision,
        CASE when clp_epoch    IS NULL then ports_epoch    else clp_epoch    END AS epoch
FROM (
  SELECT VERSIONS_VULN.*, 
        CLP.port_version  AS clp_version,
        CLP.port_revision AS clp_revision,
        CLP.port_epoch    AS clp_epoch,
        CLP.needs_refresh AS needs_refresh
FROM
(
SELECT VERSIONS.*,
       ports_vulnerable.current AS vulnerable_current,
       ports_vulnerable.past    AS vulnerable_past
FROM
( 
SELECT CLPE.*,
         P.id as port_id,
         P.forbidden,
         P.broken,
         P.deprecated,
         P.ignore,
         date_part('epoch', P.date_added)::integer AS date_added,
         P.short_description              AS short_description,
         P.category_id,
         P.revision  as ports_revision,
         P.version   as ports_version,
         P.portepoch as ports_epoch,
         P.restricted,
         P.expiration_date,
         P.no_cdrom,
         P.is_interactive,
         P.only_for_archs,
         P.not_for_archs
    FROM


  (SELECT LCPCL.*,
          CLPE.element_id,
          element_pathname(CLPE.element_id)
   FROM 
   (SELECT CL.id       AS commit_log_id, 
           commit_date AS commit_date_raw,
           message_subject,
           message_id,
           committer,
           CL.description       AS commit_description,
           to_char(CL.commit_date - SystemTimeAdjust(), 'DD Mon YYYY')  AS commit_date,
           to_char(CL.commit_date - SystemTimeAdjust(), 'HH24:MI')      AS commit_time,
           encoding_losses,
            svn_revision,
           repo.svn_hostname,
           repo.path_to_repo
    FROM commit_log CL LEFT OUTER JOIN repo ON CL.repo_id = repo.id JOIN
                (  SELECT distinct LC.commit_log_id, LC.commit_date as dummy_field
                     FROM latest_commits LC, commit_log_elements CLE, element_pathname EP
                    WHERE LC.commit_log_id = CLE.commit_log_id
                      AND CLE.element_id   = EP.element_id
                      AND EP.pathname      like Filter
                 ORDER BY LC.commit_date DESC
                    LIMIT 5) AS 
                  LCP ON CL.id = LCP.commit_log_id) AS LCPCL
                      JOIN commit_log_ports_elements CLPE on CLPE.commit_log_id = LCPCL.commit_log_id
                                                         AND CLPE.commit_log_id > latest_commits_ports_anchor()


) AS CLPE
           LEFT OUTER JOIN ports P ON CLPE.element_id = P.element_id) AS VERSIONS
           LEFT OUTER JOIN ports_vulnerable ON VERSIONS.port_id = ports_vulnerable.port_ID) AS VERSIONS_VULN
           LEFT OUTER JOIN commit_log_ports CLP ON CLP.commit_log_id = VERSIONS_VULN.commit_log_id
                                     AND VERSIONS_VULN.port_id = CLP.port_id
                                AND CLP.commit_log_id > latest_commits_ports_anchor()


) AS LCPPORTS JOIN element
on LCPPORTS.element_id = element.id) AS PORTELEMENT LEFT JOIN categories
on PORTELEMENT.category_id = categories.id)

 AS PEC) AS SECURITY LEFT OUTER JOIN
(SELECT element_id as wle_element_id, COUNT(watch_list_id) as watch
  FROM watch_list JOIN watch_list_element
        ON watch_list.id      = watch_list_element.watch_list_id
       AND watch_list.user_id = UserID
       AND watch_list.in_service
GROUP BY wle_element_id) AS W ON W.wle_element_id = SECURITY.element_id
order by commit_date_raw desc, category, port

	LOOP
        RETURN NEXT r;
    END LOOP;
    RETURN;
END
$_$;


ALTER FUNCTION public.latestcommitslargefiltered(integer, text) OWNER TO dan;

--
-- Name: latestcommitssmall(integer); Type: FUNCTION; Schema: public; Owner: dan
--

CREATE FUNCTION public.latestcommitssmall(integer) RETURNS SETOF public.commit_record
    LANGUAGE plpgsql
    AS $_$
    DECLARE
        UserID  ALIAS for $1;
        r       commit_record%rowtype;

    BEGIN
        FOR r IN SELECT * FROM LatestCommitsSmall(UserID, 'head')
        LOOP
            RETURN NEXT r;
        END LOOP;
    END;$_$;


ALTER FUNCTION public.latestcommitssmall(integer) OWNER TO dan;

--
-- Name: latestcommitssmall(integer, text); Type: FUNCTION; Schema: public; Owner: dan
--

CREATE FUNCTION public.latestcommitssmall(integer, text) RETURNS SETOF public.commit_record
    LANGUAGE plpgsql
    AS $_$
DECLARE
	UserID		ALIAS for $1;
	Branch      ALIAS for $2;

	r 	commit_record%rowtype;

BEGIN
    FOR r IN
SELECT SECURITY.commit_log_id,
       SECURITY.commit_date_raw,
       SECURITY.message_subject,
       SECURITY.message_id,
       SECURITY.committer,
       SECURITY.commit_description,
       SECURITY.commit_date,
       SECURITY.commit_time,
       SECURITY.encoding_losses,
       SECURITY.port_id,
       SECURITY.needs_refresh,
       SECURITY.forbidden,
       SECURITY.broken,
       SECURITY.deprecated,
       SECURITY.ignore,
       SECURITY.element_id,
       SECURITY.version,
       SECURITY.revision,
       SECURITY.epoch,
       SECURITY.date_added,
       SECURITY.short_description,
       SECURITY.category_id,
       SECURITY.port,
       SECURITY.status,
       SECURITY.category,
       SECURITY.vulnerable_current,
       SECURITY.vulnerable_past,
       SECURITY.restricted,
       SECURITY.no_cdrom,
       SECURITY.expiration_date,
       SECURITY.is_interactive,
       SECURITY.only_for_archs,
       SECURITY.not_for_archs,
       SECURITY.repo_name,
       SECURITY.svn_revision,
       SECURITY.svn_hostname,
       SECURITY.path_to_repo,
       W.watch,
       element_pathname(SECURITY.element_id, FALSE) as element_pathname
FROM (
SELECT PEC.*
FROM (
SELECT PORTELEMENT.*,
       categories.name AS category
FROM (
SELECT LCPPORTS.*,
       element.name    AS port,
       element.status  AS status,
        CASE WHEN port_id IS NULL THEN commit_log_id ELSE NULL END AS non_port_commit_log_id,
        CASE when clp_version  IS NULL then ports_version  else clp_version  END as version,
        CASE when clp_revision IS NULL then ports_revision else clp_revision END AS revision,
        CASE when clp_epoch    IS NULL then ports_epoch    else clp_epoch    END AS epoch
FROM (
  SELECT VERSIONS_VULN.*, 
        CLP.port_version  AS clp_version,
        CLP.port_revision AS clp_revision,
        CLP.port_epoch    AS clp_epoch,
        CLP.needs_refresh AS needs_refresh
FROM
(
SELECT VERSIONS.*,
       ports_vulnerable.current AS vulnerable_current,
       ports_vulnerable.past    AS vulnerable_past
FROM
( 
SELECT CLPE.*,
         P.id as port_id,
         P.forbidden,
         P.broken,
         P.deprecated,
         P.ignore,
         date_part('epoch', P.date_added)::integer AS date_added,
         P.short_description              AS short_description,
         P.category_id,
         P.revision  as ports_revision,
         P.version   as ports_version,
         P.portepoch as ports_epoch,
         P.restricted,
         P.expiration_date,
         P.no_cdrom,
         P.is_interactive,
         P.only_for_archs,
         P.not_for_archs
    FROM


  (SELECT LCPCL.*,
          CLPE.element_id,
          element_pathname(CLPE.element_id)
   FROM 
   (SELECT CL.id       AS commit_log_id, 
           commit_date AS commit_date_raw,
           message_subject,
           message_id,
           committer,
           CL.description AS commit_description,
           to_char(CL.commit_date - SystemTimeAdjust(), 'DD Mon YYYY')  AS commit_date,
           to_char(CL.commit_date - SystemTimeAdjust(), 'HH24:MI')      AS commit_time,
           encoding_losses,
           svn_revision,
           repo.name AS repo_name,
           repo.svn_hostname,
           repo.path_to_repo
     FROM commit_log CL LEFT OUTER JOIN repo ON CL.repo_id = repo.id JOIN
                (SELECT LCP.commit_log_id
                   FROM latest_commits_ports LCP JOIN commit_log_branches CLB ON LCP.commit_log_id = CLB.commit_log_id
                            JOIN system_branch SB ON SB.branch_name = Branch AND SB.id = CLB.branch_id
               ORDER BY LCP.commit_date DESC
                  LIMIT 10) AS 
                  LCP ON CL.id = LCP.commit_log_id) AS LCPCL
                      JOIN commit_log_ports_elements CLPE on CLPE.commit_log_id = LCPCL.commit_log_id
                                                         AND CLPE.commit_log_id > latest_commits_ports_anchor()


) AS CLPE
           LEFT OUTER JOIN ports P ON CLPE.element_id = P.element_id) AS VERSIONS
           LEFT OUTER JOIN ports_vulnerable ON VERSIONS.port_id = ports_vulnerable.port_ID) AS VERSIONS_VULN
           LEFT OUTER JOIN commit_log_ports CLP ON CLP.commit_log_id = VERSIONS_VULN.commit_log_id
                                     AND VERSIONS_VULN.port_id = CLP.port_id
                                AND CLP.commit_log_id > latest_commits_ports_anchor()


) AS LCPPORTS JOIN element
on LCPPORTS.element_id = element.id) AS PORTELEMENT LEFT JOIN categories
on PORTELEMENT.category_id = categories.id)

 AS PEC) AS SECURITY LEFT OUTER JOIN
(SELECT element_id as wle_element_id, COUNT(watch_list_id) as watch
  FROM watch_list JOIN watch_list_element
        ON watch_list.id      = watch_list_element.watch_list_id
       AND watch_list.user_id = UserID
       AND watch_list.in_service
GROUP BY wle_element_id) AS W ON W.wle_element_id = SECURITY.element_id
order by commit_date_raw desc, category, port

	LOOP
        RETURN NEXT r;
    END LOOP;
    RETURN;
END
$_$;


ALTER FUNCTION public.latestcommitssmall(integer, text) OWNER TO dan;

--
-- Name: levenshtein(text, text); Type: FUNCTION; Schema: public; Owner: dan
--

CREATE FUNCTION public.levenshtein(text, text) RETURNS integer
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/fuzzystrmatch', 'levenshtein';


ALTER FUNCTION public.levenshtein(text, text) OWNER TO dan;

--
-- Name: levenshtein(text, text, integer, integer, integer); Type: FUNCTION; Schema: public; Owner: dan
--

CREATE FUNCTION public.levenshtein(text, text, integer, integer, integer) RETURNS integer
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/fuzzystrmatch', 'levenshtein_with_costs';


ALTER FUNCTION public.levenshtein(text, text, integer, integer, integer) OWNER TO dan;

--
-- Name: logincounts(integer); Type: FUNCTION; Schema: public; Owner: dan
--

CREATE FUNCTION public.logincounts(integer) RETURNS SETOF public.logincounts_record
    LANGUAGE plpgsql
    AS $_$
DECLARE
    MaxDays ALIAS for $1;

    r   logincounts_record%rowtype;

BEGIN
    FOR X IN 1..MaxDays LOOP
        SELECT current_date -   X      * interval '1 day',
               count(*) as count
          INTO r
          FROM users
         WHERE lastlogin between current_date -   X      * interval '1 day'
                             AND current_date -  (X - 1) * interval '1 day';

        RETURN NEXT r;
    END LOOP;
    RETURN;
END
$_$;


ALTER FUNCTION public.logincounts(integer) OWNER TO dan;

--
-- Name: metaphone(text, integer); Type: FUNCTION; Schema: public; Owner: dan
--

CREATE FUNCTION public.metaphone(text, integer) RETURNS text
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/fuzzystrmatch', 'metaphone';


ALTER FUNCTION public.metaphone(text, integer) OWNER TO dan;

--
-- Name: migratestagingareatowatchlist(bigint); Type: FUNCTION; Schema: public; Owner: dan
--

CREATE FUNCTION public.migratestagingareatowatchlist(bigint) RETURNS integer
    LANGUAGE plpgsql
    AS $_$
DECLARE
	p_Watch_List_ID ALIAS FOR $1;
BEGIN
-- take everything from the staging area (for this user) and put it on the 
-- indicated watch list.
-- ignore items on the watch list which do not relate to a port
-- also ignore items in the staging area that are already on the watch list

   INSERT INTO watch_list_element
   SELECT p_Watch_List_ID, WLS.element_id
     FROM watch_list_staging WLS
 WHERE WLS.element_id IS NOT NULL
   AND WLS.user_id = (SELECT user_id FROM watch_list WHERE id = 4656 ) 
   AND NOT EXISTS ( SELECT WLE.element_id
                      FROM watch_list_element WLE, watch_list_staging WLS
                     WHERE WLE.watch_list_id = p_Watch_List_ID
                       AND WLE.element_id    = WLS.element_id
                       AND WLS.user_id       = (SELECT user_id 
                                                  FROM watch_list
                                                 WHERE id = p_Watch_List_ID));
	return 1;
END;
$_$;


ALTER FUNCTION public.migratestagingareatowatchlist(bigint) OWNER TO dan;

--
-- Name: mylogincounts(); Type: FUNCTION; Schema: public; Owner: dan
--

CREATE FUNCTION public.mylogincounts() RETURNS SETOF public.mylogincounts_record
    LANGUAGE plpgsql
    AS $$
DECLARE

    r   mylogincounts_record%rowtype;

BEGIN
        SELECT current_date,
               1234
          INTO r;
        RETURN NEXT r;
    RETURN;
END
$$;


ALTER FUNCTION public.mylogincounts() OWNER TO dan;

--
-- Name: mylogincounts(integer); Type: FUNCTION; Schema: public; Owner: dan
--

CREATE FUNCTION public.mylogincounts(integer) RETURNS SETOF public.mylogincounts_record
    LANGUAGE plpgsql
    AS $_$
DECLARE
    MaxDays ALIAS for $1;

    r   logincounts_record%rowtype;

BEGIN
        SELECT current_date,
               1234
          INTO r;
        RETURN NEXT r;
    RETURN;
END
$_$;


ALTER FUNCTION public.mylogincounts(integer) OWNER TO dan;

--
-- Name: on_delete_remove_children(); Type: FUNCTION; Schema: public; Owner: dan
--

CREATE FUNCTION public.on_delete_remove_children() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
begin

  delete from element 
   where parent_id=old.id;

  RETURN old;
end;
$$;


ALTER FUNCTION public.on_delete_remove_children() OWNER TO dan;

--
-- Name: onwatchlist(integer, integer); Type: FUNCTION; Schema: public; Owner: dan
--

CREATE FUNCTION public.onwatchlist(integer, integer) RETURNS bigint
    LANGUAGE sql STABLE
    AS $_$
	  SELECT COUNT(watch_list_id) as onwatchlist
	    FROM watch_list WL JOIN watch_list_element WLE
	      ON WL.id          = WLE.watch_list_id
	     AND WL.user_id     = $1
	     AND WL.in_service
	     AND WLE.element_id = $2
	GROUP BY element_id
$_$;


ALTER FUNCTION public.onwatchlist(integer, integer) OWNER TO dan;

--
-- Name: package_version(text, text, text); Type: FUNCTION; Schema: public; Owner: dan
--

CREATE FUNCTION public.package_version(text, text, text) RETURNS text
    LANGUAGE plpgsql IMMUTABLE
    AS $_$
DECLARE
    p_PortVersion   ALIAS for $1;
    p_PortRevision  ALIAS for $2;
    p_PortEpoch     ALIAS for $3;

    l_PKGNAME       text;
BEGIN
    l_PKGNAME := p_PortVersion;

    IF p_PortRevision <> '' AND p_PortRevision != '0' THEN
        l_PKGNAME := l_PKGNAME || '_' || p_PortRevision;
    END IF;

    IF p_PortEpoch <> '' AND p_PortEpoch != '0' THEN
        l_PKGNAME := l_PKGNAME || ',' || p_PortEpoch;
    END IF;

    RETURN l_PKGNAME;
END;
$_$;


ALTER FUNCTION public.package_version(text, text, text) OWNER TO dan;

--
-- Name: packageflavoradd(integer, text, text, integer); Type: FUNCTION; Schema: public; Owner: dan
--

CREATE FUNCTION public.packageflavoradd(integer, text, text, integer) RETURNS integer
    LANGUAGE plpgsql
    AS $_$
	DECLARE
		a_port_id       ALIAS FOR $1;
		a_flavor        ALIAS FOR $2;
		a_flavor_name   ALIAS FOR $3;
		a_flavor_number ALIAS for $4;

		l_flavor_id          BIGINT;
		l_package_flavors_id BIGINT;

	BEGIN
		INSERT INTO flavors (name) VALUES (a_flavor)
		  ON CONFLICT ON CONSTRAINT flavors_name_unique DO NOTHING
		  RETURNING id
		  INTO l_flavor_id;

		IF l_flavor_id IS NULL THEN
			SELECT id
			  INTO l_flavor_id
			  FROM flavors
			 WHERE name = a_flavor;
		END IF;

		IF NOT FOUND THEN
			RAISE EXCEPTION 'cannot find id for Flavor = % ', a_flavor;
			RETURN 0;
		END IF;
		

--		RAISE NOTICE ' flavor is %', l_flavor_id;

		INSERT INTO package_flavors (port_id, flavor_id, name, flavor_number)
			VALUES (a_port_id, l_flavor_id, a_flavor_name, a_flavor_number)
			RETURNING id
			INTO l_package_flavors_id;
		

		RETURN l_package_flavors_id;
	END;
$_$;


ALTER FUNCTION public.packageflavoradd(integer, text, text, integer) OWNER TO dan;

--
-- Name: packageflavors(bigint); Type: FUNCTION; Schema: public; Owner: dan
--

CREATE FUNCTION public.packageflavors(bigint) RETURNS SETOF public.packageflavors_record
    LANGUAGE plpgsql
    AS $_$
    DECLARE
        a_port_id  ALIAS for $1;
        r          PackageFlavors_record%rowtype;

    BEGIN
        FOR r IN SELECT PV.id, PV.port_id, PV.flavor_id, F.name as flavor_name, PV.name, PV.flavor_number
                   FROM package_flavors PV
                   JOIN flavors         F ON PV.flavor_id = F.id
                  WHERE port_id = a_port_id
               ORDER BY flavor_number ASC
        LOOP
            RETURN NEXT r;
        END LOOP;
    END;$_$;


ALTER FUNCTION public.packageflavors(bigint) OWNER TO dan;

--
-- Name: packageflavorsdelete(integer); Type: FUNCTION; Schema: public; Owner: dan
--

CREATE FUNCTION public.packageflavorsdelete(integer) RETURNS integer
    LANGUAGE plpgsql
    AS $_$
	DECLARE
		a_port_id ALIAS FOR $1;
		RowCount	int4;

	BEGIN
		DELETE FROM package_flavors
		      WHERE port_id  = a_port_id;

		GET DIAGNOSTICS RowCount = ROW_COUNT;

		RETURN RowCount;
	END
$_$;


ALTER FUNCTION public.packageflavorsdelete(integer) OWNER TO dan;

--
-- Name: packagename(integer); Type: FUNCTION; Schema: public; Owner: dan
--

CREATE FUNCTION public.packagename(integer) RETURNS text
    LANGUAGE sql STABLE
    AS $_$
  SELECT P.package_name 
  FROM ports P 
 WHERE P.id = $1
$_$;


ALTER FUNCTION public.packagename(integer) OWNER TO dan;

--
-- Name: pageloadsummaryupdate(date); Type: FUNCTION; Schema: public; Owner: dan
--

CREATE FUNCTION public.pageloadsummaryupdate(date) RETURNS integer
    LANGUAGE plpgsql
    AS $_$
   DECLARE
       l_RowCount   int;

   BEGIN
   INSERT INTO page_load_summary (date, page_name, total, users, rendering_time_min, rendering_time_max, rendering_time_avg)
      SELECT date,
             CASE WHEN page_name = '/missing.php' THEN
                CASE WHEN position('?' in full_url) != 0 THEN
                   substring(full_url from 0 for position('?' in full_url))
                ELSE
                    CASE WHEN position('cgi-bin/ads/' in full_url) = 0 THEN
                        full_url
                     ELSE
                        substring(full_url from 0 for position('%20' in full_url))
                     END
                END
                ELSE page_name
             END,
             count(*)            AS total,
             count(user_id)      AS users,
             min(rendering_time) AS min,
             max(rendering_time) AS max,
             avg(rendering_time) AS avg
        FROM page_load_detail
       WHERE date = $1
    GROUP BY 1, 2;
    
    GET DIAGNOSTICS l_RowCount = ROW_COUNT;

    return l_RowCount;

    END;
$_$;


ALTER FUNCTION public.pageloadsummaryupdate(date) OWNER TO dan;

--
-- Name: parent_must_be_directory(); Type: FUNCTION; Schema: public; Owner: dan
--

CREATE FUNCTION public.parent_must_be_directory() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
   declare
      ParentType char(1);
begin
   --  message 'parent_must_be_directory';

   if new.Parent_Id is not null then
      select directory_file_flag 
        into ParentType
       from element
      where element.id = new.parent_id;

      if (ParentType = 'F') then
         -- message 'parent cannot be a file ' || NOW(*);
         RAISE NOTICE 'modifications to element % with name=% required modifying the the parent % from a file to directory.', new.id, new.name, new.parent_id;
		UPDATE element
		   SET directory_file_flag = 'D'
		 WHERE element.id          = new.parent_id;
     end if;
   end if;

   RETURN NEW;
end;
$$;


ALTER FUNCTION public.parent_must_be_directory() OWNER TO dan;

--
-- Name: pathname_id(text); Type: FUNCTION; Schema: public; Owner: dan
--

CREATE FUNCTION public.pathname_id(text) RETURNS integer
    LANGUAGE plpgsql STABLE
    AS $_$

   DECLARE
      in_element_pathname ALIAS FOR $1;

      debug_str         text;
      element_id        int4;
      element_parent_id int4;
      element_name      text;
      element_pathname  text;
      position          int4;
      slash             text;

begin
  slash            := '/';
  element_pathname := in_element_pathname;
  element_id       := -1;

  IF substr(element_pathname,1,1) = slash THEN
    element_pathname := substr(element_pathname, 2);
  END IF;

  position := strpos(element_pathname, slash);
  IF position = 0 THEN
    element_name     := element_pathname;
    element_pathname := '';
  ELSE
    element_name     := substr(element_pathname, 1, position-1);
    element_pathname := substr(element_pathname, position+1);
  END IF;

  select id 
    into element_parent_id
    from element 
   where element.name = element_name
     and parent_id is null;

  debug_str := debug_str || '1 = ' || element_name;

  WHILE(char_length(element_pathname) > 0) LOOP
    position := strpos(element_pathname, slash);
    if position = 0 then
       element_name     := element_pathname;
       element_pathname := '';
    else
       element_name     := substr(element_pathname, 1, position-1);
       element_pathname := substr(element_pathname, position+1);
    end if;

    debug_str := debug_str || '1 = ' || element_name;
    select id 
      into element_parent_id
      from element 
     where element.name = element_name 
       and parent_id    = element_parent_id;

    debug_str := debug_str || ' 2 = ' || element_name;
    debug_str := debug_str || ' 3 = ' || element_parent_id;
  end loop;

  return element_parent_id;
END;
$_$;


ALTER FUNCTION public.pathname_id(text) OWNER TO dan;

--
-- Name: pgp_key_id(bytea); Type: FUNCTION; Schema: public; Owner: dan
--

CREATE FUNCTION public.pgp_key_id(bytea) RETURNS text
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/pgcrypto', 'pgp_key_id_w';


ALTER FUNCTION public.pgp_key_id(bytea) OWNER TO dan;

--
-- Name: pgp_pub_decrypt(bytea, bytea); Type: FUNCTION; Schema: public; Owner: dan
--

CREATE FUNCTION public.pgp_pub_decrypt(bytea, bytea) RETURNS text
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/pgcrypto', 'pgp_pub_decrypt_text';


ALTER FUNCTION public.pgp_pub_decrypt(bytea, bytea) OWNER TO dan;

--
-- Name: pgp_pub_decrypt(bytea, bytea, text); Type: FUNCTION; Schema: public; Owner: dan
--

CREATE FUNCTION public.pgp_pub_decrypt(bytea, bytea, text) RETURNS text
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/pgcrypto', 'pgp_pub_decrypt_text';


ALTER FUNCTION public.pgp_pub_decrypt(bytea, bytea, text) OWNER TO dan;

--
-- Name: pgp_pub_decrypt(bytea, bytea, text, text); Type: FUNCTION; Schema: public; Owner: dan
--

CREATE FUNCTION public.pgp_pub_decrypt(bytea, bytea, text, text) RETURNS text
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/pgcrypto', 'pgp_pub_decrypt_text';


ALTER FUNCTION public.pgp_pub_decrypt(bytea, bytea, text, text) OWNER TO dan;

--
-- Name: pgp_pub_decrypt_bytea(bytea, bytea); Type: FUNCTION; Schema: public; Owner: dan
--

CREATE FUNCTION public.pgp_pub_decrypt_bytea(bytea, bytea) RETURNS bytea
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/pgcrypto', 'pgp_pub_decrypt_bytea';


ALTER FUNCTION public.pgp_pub_decrypt_bytea(bytea, bytea) OWNER TO dan;

--
-- Name: pgp_pub_decrypt_bytea(bytea, bytea, text); Type: FUNCTION; Schema: public; Owner: dan
--

CREATE FUNCTION public.pgp_pub_decrypt_bytea(bytea, bytea, text) RETURNS bytea
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/pgcrypto', 'pgp_pub_decrypt_bytea';


ALTER FUNCTION public.pgp_pub_decrypt_bytea(bytea, bytea, text) OWNER TO dan;

--
-- Name: pgp_pub_decrypt_bytea(bytea, bytea, text, text); Type: FUNCTION; Schema: public; Owner: dan
--

CREATE FUNCTION public.pgp_pub_decrypt_bytea(bytea, bytea, text, text) RETURNS bytea
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/pgcrypto', 'pgp_pub_decrypt_bytea';


ALTER FUNCTION public.pgp_pub_decrypt_bytea(bytea, bytea, text, text) OWNER TO dan;

--
-- Name: pgp_pub_encrypt(text, bytea); Type: FUNCTION; Schema: public; Owner: dan
--

CREATE FUNCTION public.pgp_pub_encrypt(text, bytea) RETURNS bytea
    LANGUAGE c STRICT
    AS '$libdir/pgcrypto', 'pgp_pub_encrypt_text';


ALTER FUNCTION public.pgp_pub_encrypt(text, bytea) OWNER TO dan;

--
-- Name: pgp_pub_encrypt(text, bytea, text); Type: FUNCTION; Schema: public; Owner: dan
--

CREATE FUNCTION public.pgp_pub_encrypt(text, bytea, text) RETURNS bytea
    LANGUAGE c STRICT
    AS '$libdir/pgcrypto', 'pgp_pub_encrypt_text';


ALTER FUNCTION public.pgp_pub_encrypt(text, bytea, text) OWNER TO dan;

--
-- Name: pgp_pub_encrypt_bytea(bytea, bytea); Type: FUNCTION; Schema: public; Owner: dan
--

CREATE FUNCTION public.pgp_pub_encrypt_bytea(bytea, bytea) RETURNS bytea
    LANGUAGE c STRICT
    AS '$libdir/pgcrypto', 'pgp_pub_encrypt_bytea';


ALTER FUNCTION public.pgp_pub_encrypt_bytea(bytea, bytea) OWNER TO dan;

--
-- Name: pgp_pub_encrypt_bytea(bytea, bytea, text); Type: FUNCTION; Schema: public; Owner: dan
--

CREATE FUNCTION public.pgp_pub_encrypt_bytea(bytea, bytea, text) RETURNS bytea
    LANGUAGE c STRICT
    AS '$libdir/pgcrypto', 'pgp_pub_encrypt_bytea';


ALTER FUNCTION public.pgp_pub_encrypt_bytea(bytea, bytea, text) OWNER TO dan;

--
-- Name: pgp_sym_decrypt(bytea, text); Type: FUNCTION; Schema: public; Owner: dan
--

CREATE FUNCTION public.pgp_sym_decrypt(bytea, text) RETURNS text
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/pgcrypto', 'pgp_sym_decrypt_text';


ALTER FUNCTION public.pgp_sym_decrypt(bytea, text) OWNER TO dan;

--
-- Name: pgp_sym_decrypt(bytea, text, text); Type: FUNCTION; Schema: public; Owner: dan
--

CREATE FUNCTION public.pgp_sym_decrypt(bytea, text, text) RETURNS text
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/pgcrypto', 'pgp_sym_decrypt_text';


ALTER FUNCTION public.pgp_sym_decrypt(bytea, text, text) OWNER TO dan;

--
-- Name: pgp_sym_decrypt_bytea(bytea, text); Type: FUNCTION; Schema: public; Owner: dan
--

CREATE FUNCTION public.pgp_sym_decrypt_bytea(bytea, text) RETURNS bytea
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/pgcrypto', 'pgp_sym_decrypt_bytea';


ALTER FUNCTION public.pgp_sym_decrypt_bytea(bytea, text) OWNER TO dan;

--
-- Name: pgp_sym_decrypt_bytea(bytea, text, text); Type: FUNCTION; Schema: public; Owner: dan
--

CREATE FUNCTION public.pgp_sym_decrypt_bytea(bytea, text, text) RETURNS bytea
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/pgcrypto', 'pgp_sym_decrypt_bytea';


ALTER FUNCTION public.pgp_sym_decrypt_bytea(bytea, text, text) OWNER TO dan;

--
-- Name: pgp_sym_encrypt(text, text); Type: FUNCTION; Schema: public; Owner: dan
--

CREATE FUNCTION public.pgp_sym_encrypt(text, text) RETURNS bytea
    LANGUAGE c STRICT
    AS '$libdir/pgcrypto', 'pgp_sym_encrypt_text';


ALTER FUNCTION public.pgp_sym_encrypt(text, text) OWNER TO dan;

--
-- Name: pgp_sym_encrypt(text, text, text); Type: FUNCTION; Schema: public; Owner: dan
--

CREATE FUNCTION public.pgp_sym_encrypt(text, text, text) RETURNS bytea
    LANGUAGE c STRICT
    AS '$libdir/pgcrypto', 'pgp_sym_encrypt_text';


ALTER FUNCTION public.pgp_sym_encrypt(text, text, text) OWNER TO dan;

--
-- Name: pgp_sym_encrypt_bytea(bytea, text); Type: FUNCTION; Schema: public; Owner: dan
--

CREATE FUNCTION public.pgp_sym_encrypt_bytea(bytea, text) RETURNS bytea
    LANGUAGE c STRICT
    AS '$libdir/pgcrypto', 'pgp_sym_encrypt_bytea';


ALTER FUNCTION public.pgp_sym_encrypt_bytea(bytea, text) OWNER TO dan;

--
-- Name: pgp_sym_encrypt_bytea(bytea, text, text); Type: FUNCTION; Schema: public; Owner: dan
--

CREATE FUNCTION public.pgp_sym_encrypt_bytea(bytea, text, text) RETURNS bytea
    LANGUAGE c STRICT
    AS '$libdir/pgcrypto', 'pgp_sym_encrypt_bytea';


ALTER FUNCTION public.pgp_sym_encrypt_bytea(bytea, text, text) OWNER TO dan;

--
-- Name: plpgsql_call_handler(); Type: FUNCTION; Schema: public; Owner: pgsql
--

CREATE FUNCTION public.plpgsql_call_handler() RETURNS language_handler
    LANGUAGE c
    AS '$libdir/plpgsql', 'plpgsql_call_handler';


ALTER FUNCTION public.plpgsql_call_handler() OWNER TO pgsql;

SET default_tablespace = '';

SET default_table_access_method = heap;

--
-- Name: ports_conflicts_matches; Type: TABLE; Schema: public; Owner: dan
--

CREATE TABLE public.ports_conflicts_matches (
    ports_conflicts_id bigint NOT NULL,
    port_id integer NOT NULL
);


ALTER TABLE public.ports_conflicts_matches OWNER TO dan;

--
-- Name: port_conflicts(bigint, text); Type: FUNCTION; Schema: public; Owner: dan
--

CREATE FUNCTION public.port_conflicts(bigint, text) RETURNS SETOF public.ports_conflicts_matches
    LANGUAGE plpgsql STABLE
    AS $_$
  DECLARE
    a_ports_conflicts_id ALIAS FOR $1;
    a_regex              ALIAS FOR $2;
 
    l_regex text;
    r       ports_conflicts_matches%rowtype;
  BEGIN
    l_regex := '^(' || a_regex || ')';
     
    FOR r in SELECT distinct a_ports_conflicts_id, port_id FROM commit_log_ports WHERE port_name_revision ~ l_regex
    LOOP
      RETURN NEXT r;
    END LOOP;
  END;
 
$_$;


ALTER FUNCTION public.port_conflicts(bigint, text) OWNER TO dan;

--
-- Name: port_dependencies_clear_cache(); Type: FUNCTION; Schema: public; Owner: dan
--

CREATE FUNCTION public.port_dependencies_clear_cache() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
   DECLARE
      l_cache_clearing_ports_id   int8;
      l_port      text;
      l_category  text;
   BEGIN
      IF TG_OP = 'UPDATE' OR TG_OP = 'INSERT' THEN
         INSERT INTO cache_clearing_ports (port_id, category, port)
            SELECT distinct id, category, name
              FROM port_dependencies PD, ports_all PA
             WHERE PD.port_id                = NEW.id
               AND PD.port_id_dependent_upon = PA.id
               AND NOT EXISTS (SELECT port_id 
                                 FROM cache_clearing_ports
                                WHERE port_id = PD.port_id_dependent_upon);

          NOTIFY port_updated;
      END IF;

      IF TG_OP = 'DELETE' THEN
         INSERT INTO cache_clearing_ports (port_id, category, port)
            SELECT distinct id, category, name
              FROM port_dependencies PD, ports_all PA
             WHERE PD.port_id                = OLD.id
               AND PD.port_id_dependent_upon = PA.id
               AND NOT EXISTS (SELECT port_id 
                                 FROM cache_clearing_ports
                                WHERE port_id = PD.port_id_dependent_upon);

          NOTIFY port_updated;
      END IF;

      -- when a port changes, add an entry to the cache clearing table
      RETURN NEW;
   END
$$;


ALTER FUNCTION public.port_dependencies_clear_cache() OWNER TO dan;

--
-- Name: port_dependencies_delete_clear_cache(); Type: FUNCTION; Schema: public; Owner: dan
--

CREATE FUNCTION public.port_dependencies_delete_clear_cache() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
   DECLARE
      l_port      text;
      l_category  text;
   BEGIN
      --
      -- This function handles the deletion of a existing dependency.
      -- yes, we need to clear the cache for both port_id and port_id_dependent_upon
      -- from the cache_clearing_ports.  I figure there is a shorter way to this but
      -- I cannot think it through right now.
      --

      IF TG_OP = 'DELETE' THEN
            SELECT category, name
              INTO l_category, l_port
              FROM ports_all
             WHERE id = OLD.port_id;

            INSERT INTO cache_clearing_ports (port_id, category, port)
              VALUES (OLD.port_id, l_category, l_port);

            SELECT category, name
              INTO l_category, l_port
              FROM ports_all
             WHERE id = OLD.port_id_dependent_upon;

            INSERT INTO cache_clearing_ports (port_id, category, port)
              VALUES (OLD.port_id_dependent_upon, l_category, l_port);

          NOTIFY port_updated;
      END IF;

      -- when a port changes, add an entry to the cache clearing table
      RETURN OLD;
   END
$$;


ALTER FUNCTION public.port_dependencies_delete_clear_cache() OWNER TO dan;

--
-- Name: port_dependencies_insert_clear_cache(); Type: FUNCTION; Schema: public; Owner: dan
--

CREATE FUNCTION public.port_dependencies_insert_clear_cache() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
   DECLARE
      l_port      text;
      l_category  text;
   BEGIN
      --
      -- This function handles the addition of a new dependency.
      -- yes, we need to clear the cache for both port_id and port_id_dependent_upon
      -- from the cache_clearing_ports.  I figure there is a shorter way to this but
      -- I cannot think it through right now.
      --
      IF TG_OP = 'INSERT' THEN
            SELECT category, name
              INTO l_category, l_port
              FROM ports_all
             WHERE id = NEW.port_id;

            INSERT INTO cache_clearing_ports (port_id, category, port)
              VALUES (NEW.port_id, l_category, l_port);

            SELECT category, name
              INTO l_category, l_port
              FROM ports_all
             WHERE id = NEW.port_id_dependent_upon;

            INSERT INTO cache_clearing_ports (port_id, category, port)
              VALUES (NEW.port_id_dependent_upon, l_category, l_port);

          NOTIFY port_updated;
      END IF;

      -- when a port changes, add an entry to the cache clearing table
      RETURN NEW;
   END
$$;


ALTER FUNCTION public.port_dependencies_insert_clear_cache() OWNER TO dan;

--
-- Name: port_id(text, text); Type: FUNCTION; Schema: public; Owner: dan
--

CREATE FUNCTION public.port_id(text, text) RETURNS integer
    LANGUAGE sql STABLE
    AS $_$
	SELECT P.id
	  FROM element E, ports P, categories C
	WHERE E.name         = $2
	  AND E.id           = P.element_id
	  AND C.name         = $1
	  AND P.category_id  = C.id;
$_$;


ALTER FUNCTION public.port_id(text, text) OWNER TO dan;

--
-- Name: ports_categories_set(); Type: FUNCTION; Schema: public; Owner: dan
--

CREATE FUNCTION public.ports_categories_set() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
   DECLARE
		CategoryCount	integer;
		CategoryNames	text;
		Category		text;
		CategoryID		int8;
		UpdateNeeded	boolean;
   BEGIN
		UpdateNeeded := TRUE;

		IF TG_OP = 'UPDATE' THEN
			IF old.categories = new.categories THEN
				UpdateNeeded := FALSE;
			END IF;
		END IF;

		IF UpdateNeeded THEN

			DELETE 
			  FROM ports_categories
			 WHERE port_id = new.id;

			IF new.categories IS NOT NULL THEN

				CategoryCount := 0;
				CategoryNames := new.categories;
	      			LOOP
        	 			CategoryCount := CategoryCount + 1;
	        	 		Category := split_part(CategoryNames, ' ', CategoryCount);

					-- let us not get carried away....
   				      	IF Category = '' OR CategoryCount > 100 THEN
			      		      EXIT;
         				END IF;

					SELECT id
					  INTO CategoryID
					  FROM categories
					 WHERE name = Category;

					IF NOT FOUND THEN
						RAISE NOTICE ' we need to create category % ', Category;
						CategoryID := CreateCategory(Category, 'no description supplied', FALSE);
					END IF;

					INSERT INTO ports_categories (port_id, category_id)
					   SELECT ports.id, CategoryID FROM ports
					    WHERE ports.id = new.id
					      AND NOT exists (SELECT * 
					                        FROM ports_categories
					                       WHERE port_id     = new.id
					                         AND category_id = CategoryID);

--					RAISE NOTICE ' category % is % with id %', CategoryCount, Category, CategoryID;
		      		END LOOP;
			END IF;
		END IF;

      RETURN NEW;
   END
$$;


ALTER FUNCTION public.ports_categories_set() OWNER TO dan;

--
-- Name: ports_clear_cache(); Type: FUNCTION; Schema: public; Owner: dan
--

CREATE FUNCTION public.ports_clear_cache() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
   DECLARE
      l_port      text;
      l_category  text;
   BEGIN
      IF TG_OP = 'UPDATE' THEN
            SELECT category, name
              INTO l_category, l_port
              FROM ports_all
             WHERE id = NEW.id;

            INSERT INTO cache_clearing_ports (port_id, category, port)
                 VALUES (NEW.id, l_category, l_port);

          NOTIFY port_updated;
      END IF;

      -- when a port changes, add an entry to the cache clearing table
      RETURN NEW;
   END
$$;


ALTER FUNCTION public.ports_clear_cache() OWNER TO dan;

--
-- Name: ports_clear_cache_testing(); Type: FUNCTION; Schema: public; Owner: dan
--

CREATE FUNCTION public.ports_clear_cache_testing() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
BEGIN
   RAISE NOTICE 'ports_clear_cache_testing() has been invoked with %', TG_OP;
   IF TG_OP = 'UPDATE' THEN
      RAISE NOTICE 'ports_clear_cache_testing() is updating %', NEW.id;
   END IF;

   return NEW;
END
$$;


ALTER FUNCTION public.ports_clear_cache_testing() OWNER TO dan;

--
-- Name: ports_conflicts_populate(integer); Type: FUNCTION; Schema: public; Owner: dan
--

CREATE FUNCTION public.ports_conflicts_populate(integer) RETURNS integer
    LANGUAGE plpgsql
    AS $_$

  DECLARE
    a_Port_ID ALIAS FOR $1;
    l_num_rows bigint;

  BEGIN
    DELETE FROM ports_conflicts WHERE port_id = a_Port_ID;

    INSERT INTO ports_conflicts(port_id, conflicts_type, item_num, item_value)
      SELECT P.id        AS port_id, 
             'conflicts'::conflicts AS conflict_type,
             row_number() OVER (ORDER BY item_value),
             glob_to_regex(R.item_value)
        FROM ports P, regexp_split_to_table(P.conflicts, E'\\s+') AS R(item_value)
       WHERE P.id = a_Port_ID

      UNION

      SELECT P.id              AS port_id, 
             'conflicts_build'::conflicts AS conflict_type,
             row_number() OVER (ORDER BY item_value),
             glob_to_regex(R.item_value)
        FROM ports P, regexp_split_to_table(P.conflicts_build, E'\\s+') AS R(item_value)
       WHERE P.id = a_Port_ID

      UNION

      SELECT P.id                AS port_id, 
             'conflicts_install'::conflicts AS conflict_type,
             row_number() OVER (ORDER BY item_value),
             glob_to_regex(R.item_value)
        FROM ports P, regexp_split_to_table(P.conflicts_install, E'\\s+') AS R(item_value)
       WHERE P.id = a_Port_ID;

      IF FOUND THEN
         GET DIAGNOSTICS l_num_rows = ROW_COUNT;
      ELSE
         l_num_rows = 0;
      END IF;

    RETURN l_num_rows;

  END
$_$;


ALTER FUNCTION public.ports_conflicts_populate(integer) OWNER TO dan;

--
-- Name: ports_conflicts_set(); Type: FUNCTION; Schema: public; Owner: dan
--

CREATE FUNCTION public.ports_conflicts_set() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
  BEGIN
    DELETE FROM ports_conflicts WHERE port_id = NEW.id;

    INSERT INTO ports_conflicts(port_id, conflicts_type, item_num, item_value)
      SELECT NEW.id        AS port_id, 
             'conflicts'::conflicts AS conflict_type,
             row_number() OVER (ORDER BY item_value),
             glob_to_regex(R.item_value)
        FROM regexp_split_to_table(NEW.conflicts, E'\\s+') AS R(item_value)

      UNION

      SELECT NEW.id              AS port_id, 
             'conflicts_build'::conflicts AS conflict_type,
             row_number() OVER (ORDER BY item_value),
             glob_to_regex(R.item_value)
        FROM regexp_split_to_table(NEW.conflicts_build, E'\\s+') AS R(item_value)

      UNION

      SELECT NEW.id                AS port_id, 
             'conflicts_install'::conflicts AS conflict_type,
             row_number() OVER (ORDER BY item_value),
             glob_to_regex(R.item_value)
        FROM regexp_split_to_table(NEW.conflicts_install, E'\\s+') AS R(item_value);

    RETURN NEW;
  END
$$;


ALTER FUNCTION public.ports_conflicts_set() OWNER TO dan;

--
-- Name: ports_status(); Type: FUNCTION; Schema: public; Owner: dan
--

CREATE FUNCTION public.ports_status() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
   DECLARE
      l_status    text;
   BEGIN
      -- the port status always takes its value
      -- from the corresponding element
      SELECT status
        INTO l_status
        FROM element
       WHERE element.id = NEW.element_id;
       
      NEW.status := l_status;

      RETURN NEW;
   END
$$;


ALTER FUNCTION public.ports_status() OWNER TO dan;

--
-- Name: ports_vulnerable_delete_clear_cache(); Type: FUNCTION; Schema: public; Owner: dan
--

CREATE FUNCTION public.ports_vulnerable_delete_clear_cache() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
   DECLARE
      l_port      text;
      l_category  text;
   BEGIN
      IF TG_OP = 'DELETE' THEN

        SELECT category, name
          INTO l_category, l_port
          FROM ports_all
         WHERE id = OLD.port_id;

        -- if we are deleting a port, we'll get back null values for this
        IF l_category IS NOT NULL AND l_port IS NOT NULL THEN
          INSERT INTO cache_clearing_ports (port_id, category, port)
            VALUES (OLD.port_id, l_category, l_port);

          NOTIFY port_updated;
        END IF;
      END IF;

      -- when a port changes, add an entry to the cache clearing table
      RETURN OLD;
   END
$$;


ALTER FUNCTION public.ports_vulnerable_delete_clear_cache() OWNER TO dan;

--
-- Name: portsdependenciesadd(text, text, text); Type: FUNCTION; Schema: public; Owner: dan
--

CREATE FUNCTION public.portsdependenciesadd(text, text, text) RETURNS integer
    LANGUAGE plpgsql
    AS $_$
  DECLARE
    p_PortName              ALIAS FOR $1;
    p_PortNameDependentUpon ALIAS FOR $2;
    p_DependencyType        ALIAS FOR $3;

    l_PortID           int;
    l_PortIDDependency int;

  BEGIN
    select GetPort(p_PortName)
      INTO l_PortID;

    select GetPort(p_PortNameDependentUpon)
      INTO l_PortIDDependency;

    if l_PortID IS NOT NULL AND l_PortIDDependency IS NOT NULL then
      INSERT INTO port_dependencies (port_id, port_id_dependent_upon, dependency_type)
            VALUES (l_PortID, l_PortIDDependency, p_DependencyType)
            ON CONFLICT ON CONSTRAINT port_dependencies_pkey DO NOTHING;
      RETURN 1;
    else
      if l_PortID IS NULL then
        RAISE NOTICE 'cannot find this port: %', p_PortName;
      end if;
      if l_PortIDDependency IS NULL then
        RAISE NOTICE 'cannot find this port: %', p_PortNameDependentUpon;
      end if;
    end if;

    RETURN 0;
  END;
$_$;


ALTER FUNCTION public.portsdependenciesadd(text, text, text) OWNER TO dan;

--
-- Name: portsmovedadd(text, text, text, text); Type: FUNCTION; Schema: public; Owner: dan
--

CREATE FUNCTION public.portsmovedadd(text, text, text, text) RETURNS integer
    LANGUAGE plpgsql
    AS $_$
   DECLARE
      p_FromPort ALIAS FOR $1;
      p_ToPort   ALIAS FOR $2;
      p_Date     ALIAS FOR $3;
      p_Reason   ALIAS FOR $4;

      l_FromPortID	int;
      l_PortsMovedID	int;
      RC		int8;

   BEGIN
      select GetPort(p_FromPort)
        INTO l_FromPortID;

      if l_FromPortID IS NOT NULL then

         -- obtain the next number in the sequence
         l_PortsMovedID := nextval('ports_moved_id_seq');

         INSERT INTO ports_moved (id, from_port_id, to_port_id, date, reason)
             VALUES (l_PortsMovedID, l_FromPortID, GetPort(p_ToPort),
                     p_Date::date, p_Reason);

         GET DIAGNOSTICS RC = ROW_COUNT;
         IF RC = 0 THEN
            l_PortsMovedID := NULL;
         END IF;
      end if;

      RETURN l_PortsMovedID;
   END;
$_$;


ALTER FUNCTION public.portsmovedadd(text, text, text, text) OWNER TO dan;

--
-- Name: portsupdatingadd(date, text, text, text); Type: FUNCTION; Schema: public; Owner: dan
--

CREATE FUNCTION public.portsupdatingadd(date, text, text, text) RETURNS integer
    LANGUAGE plpgsql
    AS $_$
   DECLARE
      p_Date    ALIAS FOR $1;
      p_Affects ALIAS FOR $2;
      p_Author  ALIAS FOR $3;
      p_Reason  ALIAS FOR $4;

      l_PortsUpdatingID	int;
      RC		int8;

   BEGIN
      -- obtain the next number in the sequence
      l_PortsUpdatingID := nextval('ports_updating_id_seq');

      INSERT INTO ports_updating (id, date, affects, author, reason)
             VALUES (l_PortsUpdatingID, p_Date::date, p_Affects, p_Author, p_Reason);

      GET DIAGNOSTICS RC = ROW_COUNT;
      IF RC = 0 THEN
         l_PortsUpdatingID := NULL;
      END IF;

      RETURN l_PortsUpdatingID;
   END;
$_$;


ALTER FUNCTION public.portsupdatingadd(date, text, text, text) OWNER TO dan;

--
-- Name: portsupdatingportsxrefadd(integer, text); Type: FUNCTION; Schema: public; Owner: dan
--

CREATE FUNCTION public.portsupdatingportsxrefadd(integer, text) RETURNS integer
    LANGUAGE plpgsql
    AS $_$
   DECLARE
      p_PortsUpdatingID ALIAS FOR $1;
      p_Port            ALIAS FOR $2;

      l_PortID	int;
      RC	int8;

   BEGIN
      RC := 0;
      select GetPort(p_Port)
        INTO l_PortID;

--      RAISE NOTICE ' PortsUpdatingPortsXrefAdd p_PortsUpdatingID=% p_Port=% l_PortID=%', p_PortsUpdatingID, p_Port, l_PortID;
      IF l_PortID IS NOT NULL THEN
--         RAISE NOTICE ' We found that port';

         INSERT INTO ports_updating_ports_xref(ports_updating_id, port_id)
             VALUES (p_PortsUpdatingID, l_PortID);

         GET DIAGNOSTICS RC = ROW_COUNT;
      ELSE
--         RAISE NOTICE ' We DID NOT LOCATE that port';
         RC := 0;
      END IF;

--      RAISE NOTICE ' RC is %' , RC;

      RETURN RC;
   END;
$_$;


ALTER FUNCTION public.portsupdatingportsxrefadd(integer, text) OWNER TO dan;

--
-- Name: portsverifyprocess(); Type: FUNCTION; Schema: public; Owner: dan
--

CREATE FUNCTION public.portsverifyprocess() RETURNS bigint
    LANGUAGE plpgsql
    AS $$
	DECLARE
		RowCount    int8;

	BEGIN
		update ports_check 
		set category_id = (select id from categories where name = category_name);

		update ports_check set port_id = (
		select ports.id from ports, element, categories
                 WHERE ports.category_id = categories.id
                   AND ports.element_id  = element.id
                   AND categories.id     = Ports_Check.category_id
                   AND element.name      = Ports_Check.port_name);

		update ports set found_in_index = TRUE
		where exists
			(select * from ports_check where ports_check.port_id = ports.id);

		GET DIAGNOSTICS RowCount = ROW_COUNT;

		RETURN RowCount;
	END
$$;


ALTER FUNCTION public.portsverifyprocess() OWNER TO dan;

--
-- Name: portsvulnerabilitycountadjust(integer); Type: FUNCTION; Schema: public; Owner: dan
--

CREATE FUNCTION public.portsvulnerabilitycountadjust(integer) RETURNS bigint
    LANGUAGE plpgsql
    AS $_$
DECLARE
	p_PortID		ALIAS for $1;

	l_VulnCurrent	int;
	l_VulnCount		int;
	l_VulnPast		int;

	l_RowsModified	int;

BEGIN

  SELECT count(distinct vuxml_id)
    INTO l_VulnCount
    FROM commit_log_ports_vuxml
   WHERE port_id = p_PortID;

--	RAISE NOTICE 'Port % has % vulnerabilities registered', p_PortID, l_VulnCount;

	IF l_VulnCount > 0 THEN

		SELECT count(distinct vuxml_id)
		  INTO l_VulnCurrent
		  FROM commit_log_ports_vuxml CLPV, commit_log_ports CLP, ports P
		 WHERE CLP.commit_log_id = CLPV.commit_log_id
		   AND CLPV.port_id      = CLP.port_id
		   AND P.id              = CLP.port_id
		   AND P.version         = CLP.port_version
		   AND P.revision        = CLP.port_revision
		   AND CLP.port_epoch    = P.portepoch
		   AND P.id              = p_PortID;

		IF l_VulnCurrent IS NULL THEN
			l_VulnCurrent := 0;
		END IF;

--		RAISE NOTICE 'Port % has % current vulnerabilities', p_PortID, l_VulnCurrent;
		
		l_VulnPast := l_VulnCount - l_VulnCurrent;

--		RAISE NOTICE 'Port % has % past vulnerabilities', p_PortID, l_VulnPast;

		-- try an update first

		UPDATE ports_vulnerable
		   SET current = l_VulnCurrent,
		       past    = l_VulnPast
		 WHERE ports_vulnerable.port_id = p_PortID;

--		RAISE NOTICE 'UPDATE has been attempted';

		GET DIAGNOSTICS l_RowsModified = ROW_COUNT;

		-- if that fails, do an insert

		IF l_RowsModified = 0 THEN
			INSERT INTO ports_vulnerable (port_id, current, past)
			VALUES (p_PortID, l_VulnCurrent, l_VulnPast);
--			RAISE NOTICE 'new entry was inserted into ports_vulnerable';
		ELSE
--			RAISE NOTICE 'ports_vulnerable has been updated';
		END IF;
	ELSE
		-- there are no vulnerabilities for this port
		DELETE FROM ports_vulnerable where port_id = p_PortID;
	END IF;

	return l_VulnCount;

END

$_$;


ALTER FUNCTION public.portsvulnerabilitycountadjust(integer) OWNER TO dan;

--
-- Name: portverifyaddone(text, text); Type: FUNCTION; Schema: public; Owner: dan
--

CREATE FUNCTION public.portverifyaddone(text, text) RETURNS bigint
    LANGUAGE plpgsql
    AS $_$
	DECLARE
		CategoryName	ALIAS for $1;
		PortName	ALIAS for $2;
		RC		int8;

	BEGIN
		INSERT INTO Ports_Check (category_name, port_name) values (CategoryName, PortName);

		GET DIAGNOSTICS RC = ROW_COUNT;

		RETURN RC;
	END
$_$;


ALTER FUNCTION public.portverifyaddone(text, text) OWNER TO dan;

--
-- Name: portverifybegin(); Type: FUNCTION; Schema: public; Owner: dan
--

CREATE FUNCTION public.portverifybegin() RETURNS bigint
    LANGUAGE plpgsql
    AS $$
	DECLARE
		RowCount	int8;

	BEGIN
		UPDATE ports SET found_in_index = TRUE;

--		CREATE TEMP TABLE PortsCheck (category_name text, port_name text);
		DELETE FROM Ports_Check;

		GET DIAGNOSTICS RowCount = ROW_COUNT;

		RETURN RowCount;

	END
$$;


ALTER FUNCTION public.portverifybegin() OWNER TO dan;

--
-- Name: portversionlatestbeforequarterlybranch(integer); Type: FUNCTION; Schema: public; Owner: dan
--

CREATE FUNCTION public.portversionlatestbeforequarterlybranch(integer) RETURNS text
    LANGUAGE sql STABLE
    AS $_$
   -- for a port on head, pass in the id and you'll get the most recent version before the start of the current quarter
   --- by default, this will be the most recent version on the current quarterly branch, if there have been no commits to that branch for this port
   SELECT port_version
     FROM commit_log_ports CLP JOIN commit_log CL ON (CL.commit_date <= QuarterPreviousFinish() 
                                                  AND CL.id       = CLP.commit_log_id
                                                  AND CLP.port_id = $1)
 ORDER BY CL.commit_date DESC
    LIMIT 1;
$_$;


ALTER FUNCTION public.portversionlatestbeforequarterlybranch(integer) OWNER TO dan;

--
-- Name: portversiononquarterlybranch(text); Type: FUNCTION; Schema: public; Owner: dan
--

CREATE FUNCTION public.portversiononquarterlybranch(text) RETURNS text
    LANGUAGE sql STABLE
    AS $_$

-- return pkg version for $1 on current quarterly branch, if any commits

SELECT package_version(port_version, port_revision, port_epoch)
  FROM commit_log_ports CLP JOIN commit_log CL ON (CL.id       = CLP.commit_log_id
                                               AND CLP.port_id = (SELECT P.id
  FROM ports P, element_pathname EP
 WHERE EP.pathname = '/ports/branches/' || QuarterCurrent() || '/' || $1
   AND P.element_id = EP.element_id))
  JOIN commit_log_branches CLB ON (CLP.commit_log_id = CLB.commit_log_id
                               AND CLB.branch_id = QuarterCurrentBranchID())
ORDER BY CL.commit_date DESC
LIMIT 1
$_$;


ALTER FUNCTION public.portversiononquarterlybranch(text) OWNER TO dan;

--
-- Name: portversiononquarterlybranch(integer, text); Type: FUNCTION; Schema: public; Owner: dan
--

CREATE FUNCTION public.portversiononquarterlybranch(integer, text) RETURNS text
    LANGUAGE sql STABLE
    AS $_$
   -- for a given category/port and it's port id, tell me the version on the
   -- quarterly branch
   SELECT COALESCE(PortVersionOnQuarterlyBranch($2), PortVersionLatestBeforeQuarterlyBranch($1));
$_$;


ALTER FUNCTION public.portversiononquarterlybranch(integer, text) OWNER TO dan;

--
-- Name: quartercurrent(); Type: FUNCTION; Schema: public; Owner: dan
--

CREATE FUNCTION public.quartercurrent() RETURNS text
    LANGUAGE sql STABLE
    AS $$
   --
   -- returns something like 2019Q2
   --
   SELECT date_part('year', current_date) || 'Q' || date_part('quarter', current_date);
$$;


ALTER FUNCTION public.quartercurrent() OWNER TO dan;

--
-- Name: quartercurrentbranchid(); Type: FUNCTION; Schema: public; Owner: dan
--

CREATE FUNCTION public.quartercurrentbranchid() RETURNS integer
    LANGUAGE sql STABLE
    AS $$
   --
   -- return id of the current quarter branch
   --
   SELECT id FROM system_branch WHERE branch_name = QuarterCurrent() ;
$$;


ALTER FUNCTION public.quartercurrentbranchid() OWNER TO dan;

--
-- Name: quarterpreviousfinish(); Type: FUNCTION; Schema: public; Owner: dan
--

CREATE FUNCTION public.quarterpreviousfinish() RETURNS date
    LANGUAGE sql STABLE
    AS $$
   SELECT CAST(date_trunc('quarter', current_date) - interval '1 day' AS date);
$$;


ALTER FUNCTION public.quarterpreviousfinish() OWNER TO dan;

--
-- Name: quarterpreviousstart(); Type: FUNCTION; Schema: public; Owner: dan
--

CREATE FUNCTION public.quarterpreviousstart() RETURNS date
    LANGUAGE sql STABLE
    AS $$
   SELECT CAST(date_trunc('quarter', current_date) - interval '3 months' AS date)
$$;


ALTER FUNCTION public.quarterpreviousstart() OWNER TO dan;

--
-- Name: replace(character varying, character varying, character varying); Type: FUNCTION; Schema: public; Owner: dan
--

CREATE FUNCTION public.replace(character varying, character varying, character varying) RETURNS character varying
    LANGUAGE plpgsql
    AS $_$
DECLARE
subject ALIAS for $1;
match ALIAS for $2;
replace ALIAS for $3;
r varchar;
matchpos int;
remain varchar;
rempos int;
BEGIN

if (char_length(match) = 0) then
raise exception 'replace function was called with null match string. This is not permitted.';
end if;

remain := subject;
r := '';
matchpos := strpos(subject,match);
WHILE (matchpos > 0 ) LOOP
r := r || substring(remain, 0,matchpos) || replace;
rempos := matchpos + char_length(match);
remain := substring(remain,rempos); 
matchpos := strpos(remain,match);
END LOOP;

r := r || remain;
return r;

END;

$_$;


ALTER FUNCTION public.replace(character varying, character varying, character varying) OWNER TO dan;

--
-- Name: reset_password_token(text, text); Type: FUNCTION; Schema: public; Owner: dan
--

CREATE FUNCTION public.reset_password_token(text, text) RETURNS integer
    LANGUAGE plpgsql
    AS $_$
DECLARE
 in_password ALIAS for $1;
 in_token    ALIAS for $2;
 RowCount    bigint;
  
BEGIN
  UPDATE users SET password_hash = crypt( in_password, gen_salt('md5'))
    WHERE id = (SELECT user_id from user_password_reset
                 WHERE token = in_token);
  GET DIAGNOSTICS RowCount = ROW_COUNT;
  IF RowCount = 1 THEN
    DELETE FROM user_password_reset WHERE token = in_token;
  END IF;
  RETURN RowCount;
END
$_$;


ALTER FUNCTION public.reset_password_token(text, text) OWNER TO dan;

--
-- Name: revisionadd(integer, text); Type: FUNCTION; Schema: public; Owner: dan
--

CREATE FUNCTION public.revisionadd(integer, text) RETURNS boolean
    LANGUAGE plpgsql
    AS $_$
   DECLARE
      ElementID     ALIAS for $1;
      RevisionName  ALIAS for $2;

   BEGIN

      insert into element_revision (element_id, revision_name)
                  values (ElementID, RevisionName);

      return 1;
   END;
$_$;


ALTER FUNCTION public.revisionadd(integer, text) OWNER TO dan;

--
-- Name: revisionexists(integer, text); Type: FUNCTION; Schema: public; Owner: dan
--

CREATE FUNCTION public.revisionexists(integer, text) RETURNS boolean
    LANGUAGE plpgsql
    AS $_$
   DECLARE
      ElementID     ALIAS for $1;
      RevisionName  ALIAS for $2;
      TempElementID int4;
      ExistsResult  boolean;

   BEGIN
      ExistsResult := 1;

      select element_id
        into TempElementID
        from Element_Revision
       where element_id    = ElementID
         and revision_name = RevisionName;

      IF NOT FOUND THEN
         ExistsResult := 0;
      END IF;      

      return ExistsResult;
   END;
$_$;


ALTER FUNCTION public.revisionexists(integer, text) OWNER TO dan;

--
-- Name: revisionfortagget(integer, integer); Type: FUNCTION; Schema: public; Owner: dan
--

CREATE FUNCTION public.revisionfortagget(integer, integer) RETURNS text
    LANGUAGE plpgsql
    AS $_$
   DECLARE
      SystemBranchID   ALIAS for $1;
      ElementID        ALIAS for $2;
      TempRevisionName text;

-- for a given Element and TagName (System version id)
-- return the revision name assocated with it.
                            
   BEGIN
      select revision_name
        into TempRevisionName
        from system_branch_element_revision
       where system_branch_id = SystemBranchID
         and element_id       = ElementID;

      return TempRevisionName;
   END;
$_$;


ALTER FUNCTION public.revisionfortagget(integer, integer) OWNER TO dan;

--
-- Name: revisionfortagget(integer, integer, text); Type: FUNCTION; Schema: public; Owner: dan
--

CREATE FUNCTION public.revisionfortagget(integer, integer, text) RETURNS text
    LANGUAGE plpgsql
    AS $_$
   DECLARE
      SystemBranchID   ALIAS for $1;
      ElementID        ALIAS for $2;
      RevisionName     ALIAS for $3;
      TempRevisionName text;

-- for a given Element and TagName (System version id)
-- return the revision name assocated with it.
                            
   BEGIN
      select revision_name
        into TempRevisionName
        from system_branch_element_revision
       where system_branch_id = SystemBranchID
         and element_id       = ElementID
         and revision_name    = RevisionName;

      return TempRevisionName;
   END;
$_$;


ALTER FUNCTION public.revisionfortagget(integer, integer, text) OWNER TO dan;

--
-- Name: sanitytestfailures(integer); Type: FUNCTION; Schema: public; Owner: dan
--

CREATE FUNCTION public.sanitytestfailures(integer) RETURNS SETOF public.commit_record
    LANGUAGE plpgsql
    AS $_$
DECLARE
	p_UserID		ALIAS for $1;

	r 	commit_record%rowtype;

BEGIN
    FOR r IN
SELECT CL.id           AS commit_log_id,
       CL.commit_date  AS commit_date_raw,
       CL.message_subject,
       CL.message_id,
       CL.committer,
       CL.description  AS commit_description,
       to_char(CL.commit_date - SystemTimeAdjust(), 'DD Mon YYYY')  AS commit_date,
       to_char(CL.commit_date - SystemTimeAdjust(), 'HH24:MI')      AS commit_time,
       CL.encoding_losses,
       P.id            AS port_id,
       CLP.needs_refresh,
       P.forbidden,
       P.broken,
       P.deprecated,
       P.ignore,
       P.element_id,
       CLP.port_version  AS version,
       CLP.port_revision AS revision,
       CLP.port_epoch    AS epoch,
       date_part('epoch', P.date_added)::integer AS date_added,
       P.short_description,
       P.category_id,
       E.name AS port,
       P.status,
       C.name AS category,
       PV.current AS vulnerable_current,
       PV.past    AS vulnerable_past,
       P.restricted,
       P.no_cdrom,
       P.expiration_date,
       P.is_interactive,
       P.only_for_archs,
       P.not_for_archs,
       repo.name,
       CL.svn_revision,
       repo.svn_hostname,
       repo.path_to_repo,
       0 AS watch,
       EP.pathname AS element_pathname
  FROM commit_log           CL LEFT OUTER JOIN repo ON CL.repo_id = repo.id,
       commit_log_ports     CLP,
       sanity_test_failures STF, 
       element              E,
       categories           C,
       element_pathname     EP,
       ports                P
       LEFT OUTER JOIN 
       ports_vulnerable     PV ON P.id = PV.port_id
       LEFT OUTER JOIN (
		SELECT element_id as wle_element_id, COUNT(watch_list_id) as watch
		  FROM watch_list JOIN watch_list_element
		    ON watch_list.id      = watch_list_element.watch_list_id
		   AND watch_list.user_id = p_UserID
		   AND watch_list.in_service
		GROUP BY wle_element_id) AS W ON W.wle_element_id = P.element_id
WHERE CL.id         = STF.commit_log_id
   AND CLP.commit_log_id = CL.id
   AND CLP.port_id   = P.id
   AND P.element_id  = E.id
   AND P.element_id  = EP.element_id
   AND P.category_id = C.id
	LOOP
        RETURN NEXT r;
    END LOOP;
    RETURN;
END
$_$;


ALTER FUNCTION public.sanitytestfailures(integer) OWNER TO dan;

--
-- Name: security_notice_audit(); Type: FUNCTION; Schema: public; Owner: dan
--

CREATE FUNCTION public.security_notice_audit() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
BEGIN
   INSERT INTO security_notice_audit
      (security_notice_id,
       user_id,
       date_added,
       ip_address,
       description,
       commit_log_id,
       security_notice_status_id)
   VALUES (
       OLD.id,
       OLD.user_id,
       OLD.date_added,
       OLD.ip_address,
       OLD.description,
       OLD.commit_log_id,
       OLD.security_notice_status_id);

   NEW.date_added = CURRENT_TIMESTAMP;

   RETURN NEW;
END;

$$;


ALTER FUNCTION public.security_notice_audit() OWNER TO dan;

--
-- Name: securitynoticecreate(integer, inet, text, text); Type: FUNCTION; Schema: public; Owner: dan
--

CREATE FUNCTION public.securitynoticecreate(integer, inet, text, text) RETURNS bigint
    LANGUAGE plpgsql
    AS $_$
   DECLARE
      UserID       ALIAS for $1;
      IPAddress    ALIAS for $2;
      NoticeMsg    ALIAS for $3;
      MessageID    ALIAS for $4;

      CommitLogID       int8;
      SecurityNoticeID  int8;

   BEGIN
      SELECT id, commit_log_id
        INTO SecurityNoticeID, CommitLogID
        FROM security_notice
       WHERE commit_log_id =
                (SELECT id
                   FROM commit_log
                  WHERE message_id = MessageID
                 );

      IF FOUND THEN
			RAISE NOTICE ' Updating security notice for % MessageID User % from % said %', MessageID, UserId, IPAddress, NoticeMsg;
         UPDATE security_notice
            SET user_id       = UserID,
                ip_address    = IPAddress,
                description   = NoticeMsg
          WHERE id            = SecurityNoticeID;
      ELSE
         SELECT id
           INTO CommitLogID
           FROM commit_log
          WHERE message_id = MessageID;

         IF FOUND THEN
            -- obtain the next number in the sequence
            SecurityNoticeID := nextval('security_notice_id_seq');

   			RAISE NOTICE ' Creating security notice % for % MessageID User % from % said %', SecurityNoticeID, MessageID, UserId, IPAddress, NoticeMsg;

            INSERT INTO security_notice
                   (id,
                    user_id,
                    ip_address,
                    description,
                    commit_log_id)
            VALUES (SecurityNoticeID,
                    UserID,
                    IPAddress,
                    NoticeMsg,
                    CommitLogID);
         ELSE
            RAISE NOTICE 'attempt to insert security notice for unknown message % by User % from % message %', MessageID, UserID, IPAddress, NoticeMsg;
         END IF;
      END IF;

      return SecurityNoticeID;

   END
$_$;


ALTER FUNCTION public.securitynoticecreate(integer, inet, text, text) OWNER TO dan;

--
-- Name: securitynoticecreate(integer, inet, text, text, text); Type: FUNCTION; Schema: public; Owner: dan
--

CREATE FUNCTION public.securitynoticecreate(integer, inet, text, text, text) RETURNS bigint
    LANGUAGE plpgsql
    AS $_$
   DECLARE
      UserID       ALIAS for $1;
      IPAddress    ALIAS for $2;
      NoticeMsg    ALIAS for $3;
      MessageID    ALIAS for $4;
      p_Status     ALIAS for $5;
      CommitLogID       int8;
      SecurityNoticeID  int8;

   BEGIN
      SELECT id, commit_log_id
        INTO SecurityNoticeID, CommitLogID
        FROM security_notice
       WHERE commit_log_id =
                (SELECT id
                   FROM commit_log
                  WHERE message_id = MessageID
                 );

      IF FOUND THEN
			RAISE NOTICE ' Updating security notice for % MessageID User % from % said %', MessageID, UserId, IPAddress, NoticeMsg;
         UPDATE security_notice
            SET user_id                   = UserID,
                ip_address                = IPAddress,
                description               = NoticeMsg,
                security_notice_status_id = p_Status
          WHERE id                        = SecurityNoticeID;
      ELSE
         SELECT id
           INTO CommitLogID
           FROM commit_log
          WHERE message_id = MessageID;

         IF FOUND THEN
            -- obtain the next number in the sequence
            SecurityNoticeID := nextval('security_notice_id_seq');

   			RAISE NOTICE ' Creating security notice % for % MessageID User % from % said %', SecurityNoticeID, MessageID, UserId, IPAddress, NoticeMsg;

            INSERT INTO security_notice
                   (id,
                    user_id,
                    ip_address,
                    description,
                    commit_log_id,
                    security_notice_status_id)
            VALUES (SecurityNoticeID,
                    UserID,
                    IPAddress,
                    NoticeMsg,
                    CommitLogID,
                    p_Status);
         ELSE
            RAISE NOTICE 'attempt to insert security notice for unknown message % by User % from % message %', MessageID, UserID, IPAddress, NoticeMsg;
         END IF;
      END IF;

      return SecurityNoticeID;

   END
$_$;


ALTER FUNCTION public.securitynoticecreate(integer, inet, text, text, text) OWNER TO dan;

--
-- Name: setslaveport(text, text); Type: FUNCTION; Schema: public; Owner: dan
--

CREATE FUNCTION public.setslaveport(text, text) RETURNS integer
    LANGUAGE plpgsql
    AS $_$
--
-- Takes two category/port values and makes the second a slave
-- of the second
--
	DECLARE
		p_SlaveCategoryPort		ALIAS FOR $1;
		p_MasterCategoryPort	ALIAS FOR $2;

		l_MasterPortID			int4;
		l_SlavePortID			int4;

		l_MasterPortCategory	text;
		l_MasterPortName		text;

		l_SlavePortCategory     text;
		l_SlavePortName         text;

		position				int;

	BEGIN
		position := strpos(p_SlaveCategoryPort, '/');
		l_SlavePortCategory := substr(p_SlaveCategoryPort, 1, position - 1);
		l_SlavePortName     := substr(p_SlaveCategoryPort, position + 1);

		l_SlavePortID := GetPort(l_SlavePortCategory, l_SlavePortName);
		IF (l_SlavePortID IS NULL) THEN
			RAISE EXCEPTION 'cannot find port id for category = % port = % (%)', l_SlavePortCategory, l_SlavePortName, p_SlaveCategoryPort;
		END IF;

		position := strpos(p_MasterCategoryPort, '/');
		l_MasterPortCategory := substr(p_MasterCategoryPort, 1, position - 1);
		l_MasterPortName     := substr(p_MasterCategoryPort, position + 1);

		l_MasterPortID := GetPort(l_MasterPortCategory, l_MasterPortName);

		IF (l_MasterPortID IS NULL) THEN
			RAISE EXCEPTION 'cannot find port id for category = % port = % (%)', l_MasterPortCategory, l_MasterPortName, p_MasterCategoryPort;
		END IF;

		DELETE FROM master_slave_ports
		WHERE  slave_port_id = l_SlavePortID;

		UPDATE ports
		   SET is_slave_port = TRUE
		 WHERE id            = l_SlavePortID;

		INSERT INTO master_slave_ports (master_port_id, slave_port_id)
			VALUES (l_MasterPortID, l_SlavePortID);

		RETURN l_MasterPortID;
	END
$_$;


ALTER FUNCTION public.setslaveport(text, text) OWNER TO dan;

--
-- Name: soundex(text); Type: FUNCTION; Schema: public; Owner: dan
--

CREATE FUNCTION public.soundex(text) RETURNS text
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/fuzzystrmatch', 'soundex';


ALTER FUNCTION public.soundex(text) OWNER TO dan;

--
-- Name: stats_categorycount(); Type: FUNCTION; Schema: public; Owner: dan
--

CREATE FUNCTION public.stats_categorycount() RETURNS bigint
    LANGUAGE plpgsql
    AS $$
	DECLARE
		CategoryCount	int8;

	BEGIN
		SELECT count(*)
		  INTO CategoryCount
		  FROM categories;

		return CategoryCount;
	END
$$;


ALTER FUNCTION public.stats_categorycount() OWNER TO dan;

--
-- Name: stats_commitcount(); Type: FUNCTION; Schema: public; Owner: dan
--

CREATE FUNCTION public.stats_commitcount() RETURNS bigint
    LANGUAGE plpgsql
    AS $$
	DECLARE
		RowCount	int8;

	BEGIN
		select count(*) 
		  INTO RowCount
		  from commit_log;

		return RowCount;
	END
$$;


ALTER FUNCTION public.stats_commitcount() OWNER TO dan;

--
-- Name: stats_commitcountdays(); Type: FUNCTION; Schema: public; Owner: dan
--

CREATE FUNCTION public.stats_commitcountdays() RETURNS bigint
    LANGUAGE plpgsql
    AS $$
	DECLARE
		RowCount	int8;

	BEGIN
		select count(distinct date_trunc('day', commit_date))
		  INTO RowCount
		  from commit_log;

		return RowCount;
	END
$$;


ALTER FUNCTION public.stats_commitcountdays() OWNER TO dan;

--
-- Name: stats_commitcountdaysports(); Type: FUNCTION; Schema: public; Owner: dan
--

CREATE FUNCTION public.stats_commitcountdaysports() RETURNS bigint
    LANGUAGE plpgsql
    AS $$
	DECLARE
		RowCount	int8;

	BEGIN
		select count(distinct date_trunc('day', commit_date))
		  INTO RowCount
		  from commit_log, commit_log_ports
		 where commit_log_ports.commit_log_id = commit_log.id;

		return RowCount;
	END
$$;


ALTER FUNCTION public.stats_commitcountdaysports() OWNER TO dan;

--
-- Name: stats_commitcountports(); Type: FUNCTION; Schema: public; Owner: dan
--

CREATE FUNCTION public.stats_commitcountports() RETURNS bigint
    LANGUAGE plpgsql
    AS $$
	DECLARE
		RowCount	int8;

	BEGIN
		select count(distinct commit_log_id) 
		  INTO RowCount
		  from commit_log_ports;

		return RowCount;
	END
$$;


ALTER FUNCTION public.stats_commitcountports() OWNER TO dan;

--
-- Name: stats_commitlogcount(); Type: FUNCTION; Schema: public; Owner: dan
--

CREATE FUNCTION public.stats_commitlogcount() RETURNS bigint
    LANGUAGE plpgsql
    AS $$
	DECLARE
		RowCount	int8;

	BEGIN
		select count(*) 
		  INTO RowCount
		  from commit_log_ports;

		return RowCount;
	END
$$;


ALTER FUNCTION public.stats_commitlogcount() OWNER TO dan;

--
-- Name: stats_databasesize(); Type: FUNCTION; Schema: public; Owner: dan
--

CREATE FUNCTION public.stats_databasesize() RETURNS bigint
    LANGUAGE sql STABLE
    AS $$
	SELECT pg_database_size(current_database());
$$;


ALTER FUNCTION public.stats_databasesize() OWNER TO dan;

--
-- Name: stats_expiration(); Type: FUNCTION; Schema: public; Owner: dan
--

CREATE FUNCTION public.stats_expiration() RETURNS bigint
    LANGUAGE plpgsql
    AS $$
	DECLARE
		ExpirationCount	int8;

	BEGIN
             SELECT COUNT(*)
	       INTO ExpirationCount
               FROM (
                SELECT element_pathname(NP.element_id)
                  FROM (
                SELECT P.id, P.last_commit_id, P.element_id,  P.expiration_date
                  FROM ports P JOIN element E ON P.expiration_date IS NOT NULL and P.element_id = E.id and E.status = 'A') AS NP
                GROUP BY NP.element_id
                HAVING element_pathname(NP.element_id) ilike '/ports/head/%') AS paths;
		-- adding the HAVING was faster than doing the regular stuff (seeStats_Expired()).

		return ExpirationCount;
	END
$$;


ALTER FUNCTION public.stats_expiration() OWNER TO dan;

--
-- Name: stats_expired(); Type: FUNCTION; Schema: public; Owner: dan
--

CREATE FUNCTION public.stats_expired() RETURNS bigint
    LANGUAGE plpgsql
    AS $$
	DECLARE
		ExpirationCount	int8;

	BEGIN
		SELECT count(*)
		  INTO ExpirationCount
		  FROM (
		SELECT P.id, P.last_commit_id
		  FROM ports_active P 
                 WHERE CURRENT_DATE > P.expiration_date) AS NP
			LEFT OUTER JOIN commit_log          CL  ON NP.last_commit_id = CL.id
                                   JOIN commit_log_ports    CLP ON CLP.commit_log_id = CL.id AND NP.id = CLP.port_id
                                   JOIN commit_log_branches CLB ON CLP.commit_log_id = CLB.commit_log_id
                                   JOIN system_branch       SB  ON SB.branch_name    = 'head' AND SB.id = CLB.branch_id;


		return ExpirationCount;
	END
$$;


ALTER FUNCTION public.stats_expired() OWNER TO dan;

--
-- Name: stats_interactive(); Type: FUNCTION; Schema: public; Owner: dan
--

CREATE FUNCTION public.stats_interactive() RETURNS bigint
    LANGUAGE plpgsql
    AS $$
	DECLARE
		InteractiveCount	int8;

	BEGIN
		SELECT count(*)
		  INTO InteractiveCount
		  FROM ports_active
		 WHERE ports_active.is_interactive != '';

		return InteractiveCount;
	END
$$;


ALTER FUNCTION public.stats_interactive() OWNER TO dan;

--
-- Name: stats_portcount(); Type: FUNCTION; Schema: public; Owner: dan
--

CREATE FUNCTION public.stats_portcount() RETURNS bigint
    LANGUAGE plpgsql
    AS $$
	DECLARE
		PortCount	int8;

	BEGIN
		SELECT count(*)
		  INTO PortCount
		  FROM ports, element
		 WHERE element.status = 'A'
		   AND ports.element_id = element.id;

		return PortCount;
	END
$$;


ALTER FUNCTION public.stats_portcount() OWNER TO dan;

--
-- Name: stats_portcountbroken(); Type: FUNCTION; Schema: public; Owner: dan
--

CREATE FUNCTION public.stats_portcountbroken() RETURNS bigint
    LANGUAGE plpgsql
    AS $$
	DECLARE
		PortCount	int8;

	BEGIN
             SELECT COUNT(*)
	       INTO PortCount
               FROM (
                SELECT element_pathname(NP.element_id)
                  FROM (
                SELECT P.element_id
                  FROM ports P JOIN element E ON P.element_id = E.id AND E.status = 'A' AND P.broken <> '') AS NP
                GROUP BY NP.element_id
                HAVING element_pathname(NP.element_id) ilike '/ports/head/%') AS paths;

		return PortCount;
	END
$$;


ALTER FUNCTION public.stats_portcountbroken() OWNER TO dan;

--
-- Name: stats_portcountdeleted(); Type: FUNCTION; Schema: public; Owner: dan
--

CREATE FUNCTION public.stats_portcountdeleted() RETURNS bigint
    LANGUAGE plpgsql
    AS $$
	DECLARE
		PortCount	int8;

	BEGIN
		SELECT count(*)
		  INTO PortCount
		  FROM ports, element
		 WHERE element.status = 'D'
		   AND ports.element_id = element.id;

		return PortCount;
	END
$$;


ALTER FUNCTION public.stats_portcountdeleted() OWNER TO dan;

--
-- Name: stats_portcountdeprecated(); Type: FUNCTION; Schema: public; Owner: dan
--

CREATE FUNCTION public.stats_portcountdeprecated() RETURNS bigint
    LANGUAGE plpgsql
    AS $$
	DECLARE
		PortCount	int8;

	BEGIN
             SELECT COUNT(*)
	       INTO PortCount
               FROM (
                SELECT element_pathname(NP.element_id)
                  FROM (
                SELECT P.element_id
                  FROM ports P JOIN element E ON P.element_id = E.id AND E.status = 'A' AND P.deprecated <> '') AS NP
                GROUP BY NP.element_id
                HAVING element_pathname(NP.element_id) ilike '/ports/head/%') AS paths;

		return PortCount;
	END
$$;


ALTER FUNCTION public.stats_portcountdeprecated() OWNER TO dan;

--
-- Name: stats_portcountforbidden(); Type: FUNCTION; Schema: public; Owner: dan
--

CREATE FUNCTION public.stats_portcountforbidden() RETURNS bigint
    LANGUAGE plpgsql
    AS $$
	DECLARE
		PortCount	int8;

	BEGIN
		SELECT count(*)
		  INTO PortCount
		  FROM ports, element
		 WHERE element.status   = 'A'
		   AND ports.element_id = element.id
		   AND ports.forbidden <> '';

		return PortCount;
	END
$$;


ALTER FUNCTION public.stats_portcountforbidden() OWNER TO dan;

--
-- Name: stats_portcountignore(); Type: FUNCTION; Schema: public; Owner: dan
--

CREATE FUNCTION public.stats_portcountignore() RETURNS bigint
    LANGUAGE plpgsql
    AS $$
	DECLARE
		PortCount	int8;

	BEGIN

             SELECT COUNT(*)
	       INTO PortCount
               FROM (
                SELECT element_pathname(NP.element_id)
                  FROM (
                SELECT P.element_id
                  FROM ports P JOIN element E ON P.element_id = E.id AND E.status = 'A' AND P.ignore <> '') AS NP
                GROUP BY NP.element_id
                HAVING element_pathname(NP.element_id) ilike '/ports/head/%') AS paths;

		return PortCount;
	END
$$;


ALTER FUNCTION public.stats_portcountignore() OWNER TO dan;

--
-- Name: stats_portcountnewinterval(interval); Type: FUNCTION; Schema: public; Owner: dan
--

CREATE FUNCTION public.stats_portcountnewinterval(interval) RETURNS bigint
    LANGUAGE plpgsql
    AS $_$
	DECLARE
		SinceWhen	ALIAS FOR $1;
		PortCount	int8;

	BEGIN
		SELECT count(*)
		  INTO PortCount
		  FROM ports P
		  JOIN element_pathname EP ON P.element_id = EP.element_id
		 WHERE P.date_added > now() - SinceWhen
		  AND EP.pathname LIKE '/ports/head/%';

		return PortCount;
	END
$_$;


ALTER FUNCTION public.stats_portcountnewinterval(interval) OWNER TO dan;

--
-- Name: stats_portcountnewthisweek(); Type: FUNCTION; Schema: public; Owner: dan
--

CREATE FUNCTION public.stats_portcountnewthisweek() RETURNS bigint
    LANGUAGE plpgsql
    AS $$
	DECLARE
		PortCount	int8;

	BEGIN
		SELECT Stats_PortCountNewInterval(interval '7 days')
		  INTO PortCount;

		return PortCount;
	END
$$;


ALTER FUNCTION public.stats_portcountnewthisweek() OWNER TO dan;

--
-- Name: stats_portcountnewtoday(); Type: FUNCTION; Schema: public; Owner: dan
--

CREATE FUNCTION public.stats_portcountnewtoday() RETURNS bigint
    LANGUAGE plpgsql
    AS $$
	DECLARE
		PortCount	int8;

	BEGIN
		SELECT Stats_PortCountNewInterval(interval '1 day')
		  INTO PortCount;

		return PortCount;
	END
$$;


ALTER FUNCTION public.stats_portcountnewtoday() OWNER TO dan;

--
-- Name: stats_portcountnewyesterday(); Type: FUNCTION; Schema: public; Owner: dan
--

CREATE FUNCTION public.stats_portcountnewyesterday() RETURNS bigint
    LANGUAGE plpgsql
    AS $$
	DECLARE
		PortCount	int8;

	BEGIN
		SELECT Stats_PortCountNewInterval(interval '2 days')
		  INTO PortCount;

		return PortCount;
	END
$$;


ALTER FUNCTION public.stats_portcountnewyesterday() OWNER TO dan;

--
-- Name: stats_portcountnocdrom(); Type: FUNCTION; Schema: public; Owner: dan
--

CREATE FUNCTION public.stats_portcountnocdrom() RETURNS bigint
    LANGUAGE plpgsql
    AS $$
	DECLARE
		PortCount	int8;

	BEGIN
             SELECT COUNT(*)
               INTO PortCount
               FROM (
                SELECT element_pathname(NP.element_id)
                  FROM (
                SELECT P.element_id
                  FROM ports P JOIN element E ON P.element_id = E.id AND E.status = 'A' AND P.no_cdrom <> '') AS NP
                GROUP BY NP.element_id
                HAVING element_pathname(NP.element_id) ilike '/ports/head/%') AS paths;

		return PortCount;
	END
$$;


ALTER FUNCTION public.stats_portcountnocdrom() OWNER TO dan;

--
-- Name: stats_portcountrestricted(); Type: FUNCTION; Schema: public; Owner: dan
--

CREATE FUNCTION public.stats_portcountrestricted() RETURNS bigint
    LANGUAGE plpgsql
    AS $$
	DECLARE
		PortCount	int8;

	BEGIN
             SELECT COUNT(*)
	       INTO PortCount
               FROM (
                SELECT element_pathname(NP.element_id)
                  FROM (
                SELECT P.element_id
                  FROM ports P JOIN element E ON P.element_id = E.id AND E.status = 'A' AND P.restricted <> '') AS NP
                GROUP BY NP.element_id
                HAVING element_pathname(NP.element_id) ilike '/ports/head/%') AS paths;

		return PortCount;
	END
$$;


ALTER FUNCTION public.stats_portcountrestricted() OWNER TO dan;

--
-- Name: stats_portcountvulnerable(); Type: FUNCTION; Schema: public; Owner: dan
--

CREATE FUNCTION public.stats_portcountvulnerable() RETURNS bigint
    LANGUAGE plpgsql
    AS $$
	DECLARE
		PortCount	int8;

	BEGIN
                SELECT count(*)
		  INTO PortCount
                  FROM ports            P
		  JOIN ports_vulnerable PV ON PV.current    > 0            AND PV.port_id = P.id
                  JOIN element_pathname EP ON EP.element_id = P.element_id AND EP.pathname like '/ports/head/%'
                  JOIN element          E  ON P.element_id  = E.id         AND E.status = 'A';

		return PortCount;
	END
$$;


ALTER FUNCTION public.stats_portcountvulnerable() OWNER TO dan;

--
-- Name: stats_usercount(); Type: FUNCTION; Schema: public; Owner: dan
--

CREATE FUNCTION public.stats_usercount() RETURNS bigint
    LANGUAGE plpgsql
    AS $$
	DECLARE
		RowCount	int8;

	BEGIN
		select count(*)
		  INTO RowCount
		  from users;

		return RowCount;
	END
$$;


ALTER FUNCTION public.stats_usercount() OWNER TO dan;

--
-- Name: stats_watchlistcount(); Type: FUNCTION; Schema: public; Owner: dan
--

CREATE FUNCTION public.stats_watchlistcount() RETURNS bigint
    LANGUAGE plpgsql
    AS $$ 
	DECLARE
		RowCount	int8;

	BEGIN
		select count(*)
		  INTO RowCount
		  from watch_list;

		return RowCount;
	END
$$;


ALTER FUNCTION public.stats_watchlistcount() OWNER TO dan;

--
-- Name: stats_watchlistelementcount(); Type: FUNCTION; Schema: public; Owner: dan
--

CREATE FUNCTION public.stats_watchlistelementcount() RETURNS bigint
    LANGUAGE plpgsql
    AS $$ 
	DECLARE
		RowCount	int8;

	BEGIN
		select count(*)
		  INTO RowCount
		  from watch_list_element;

		return RowCount;
	END
$$;


ALTER FUNCTION public.stats_watchlistelementcount() OWNER TO dan;

--
-- Name: systembranchidget(integer, text); Type: FUNCTION; Schema: public; Owner: dan
--

CREATE FUNCTION public.systembranchidget(integer, text) RETURNS integer
    LANGUAGE plpgsql
    AS $_$
   DECLARE
      a_System_ID           ALIAS for $1;
      a_System_Branch_Name  ALIAS for $2;
      l_System_Branch_ID    int4;

   BEGIN
      select id
        into l_System_Branch_ID
        from system_branch
       where system_id    = a_System_ID
         and branch_name  = a_System_Branch_Name;

      return l_System_Branch_ID;
   END;
$_$;


ALTER FUNCTION public.systembranchidget(integer, text) OWNER TO dan;

--
-- Name: systemidget(text); Type: FUNCTION; Schema: public; Owner: dan
--

CREATE FUNCTION public.systemidget(text) RETURNS integer
    LANGUAGE plpgsql
    AS $_$
   DECLARE
      SystemName ALIAS for $1;
      SystemID   int4;

   BEGIN
      select id
        into SystemID
        from system
       where name = SystemName;

      return SystemID;
   END;
$_$;


ALTER FUNCTION public.systemidget(text) OWNER TO dan;

--
-- Name: systemtimeadjust(); Type: FUNCTION; Schema: public; Owner: dan
--

CREATE FUNCTION public.systemtimeadjust() RETURNS interval
    LANGUAGE sql IMMUTABLE
    AS $$
	SELECT time_adjust
	  FROM system
	 LIMIT 1;
$$;


ALTER FUNCTION public.systemtimeadjust() OWNER TO dan;

--
-- Name: text_soundex(text); Type: FUNCTION; Schema: public; Owner: dan
--

CREATE FUNCTION public.text_soundex(text) RETURNS text
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/fuzzystrmatch', 'soundex';


ALTER FUNCTION public.text_soundex(text) OWNER TO dan;

--
-- Name: user_email_change(); Type: FUNCTION; Schema: public; Owner: dan
--

CREATE FUNCTION public.user_email_change() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
begin

	if old.email <> new.email then
       RAISE NOTICE 'User % is changing email from % to %', new.name, old.email, new.email;
    end if;

   -- return values from triggers fired AFTER are ignored.
   RETURN NULL;
end;
$$;


ALTER FUNCTION public.user_email_change() OWNER TO dan;

--
-- Name: user_password_reset_purge(); Type: FUNCTION; Schema: public; Owner: dan
--

CREATE FUNCTION public.user_password_reset_purge() RETURNS integer
    LANGUAGE plpgsql
    AS $$
DECLARE
 RowCount int8;
  
BEGIN
 DELETE FROM user_password_reset WHERE date_requested < now() - interval '2 days';
 GET DIAGNOSTICS RowCount = ROW_COUNT;
 RETURN RowCount;
END
$$;


ALTER FUNCTION public.user_password_reset_purge() OWNER TO dan;

--
-- Name: user_password_reset_token(); Type: FUNCTION; Schema: public; Owner: dan
--

CREATE FUNCTION public.user_password_reset_token() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
  BEGIN
    NEW.token := ENCODE(DIGEST((random() * NEW.user_id )::text || now(), 'sha256'), 'hex');
    RETURN NEW;
  END
$$;


ALTER FUNCTION public.user_password_reset_token() OWNER TO dan;

--
-- Name: user_watch_list_create(); Type: FUNCTION; Schema: public; Owner: dan
--

CREATE FUNCTION public.user_watch_list_create() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
begin

	insert into watch_list (user_id, name, in_service) 
                    values (new.id, 'main', TRUE);

	insert into user_confirmations (user_id, token)
					values (new.id, ENCODE(DIGEST((random() * 105311100709)::text, 'md5'), 'hex'));

   RETURN OLD;
end;
$$;


ALTER FUNCTION public.user_watch_list_create() OWNER TO dan;

--
-- Name: version_revision_uniqueness(); Type: FUNCTION; Schema: public; Owner: dan
--

CREATE FUNCTION public.version_revision_uniqueness() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
   declare 
      ElementID            int4;
      ExistingRevisionName text;
begin

   select revision_name
     into ExistingRevisionName
     from system_branch_element_revision
    where system_branch_id = new.system_branch_id
      and element_id        = new.element_id;

   IF FOUND THEN
      delete from system_branch_element_revision 
       where system_branch_id = new.system_branch_id
         and element_id       = new.element_id;
   END IF;

   RETURN NEW;

end;
$$;


ALTER FUNCTION public.version_revision_uniqueness() OWNER TO dan;

--
-- Name: vuxml_ranges(); Type: FUNCTION; Schema: public; Owner: dan
--

CREATE FUNCTION public.vuxml_ranges() RETURNS SETOF public.vuxml_record
    LANGUAGE sql STABLE
    AS $$
SELECT V.id,
       V.vid,
       VN.name      AS package_name,
       VR.operator1 AS op1,
       VR.version1  AS v1,
       VR.operator2 AS op2,
       VR.version2  AS v2
  FROM vuxml_ranges VR, vuxml_names VN, vuxml_affected VA, vuxml V
 WHERE V.id                 = VA.vuxml_id
   AND VN.vuxml_affected_id = VA.id
   AND VR.vuxml_affected_id = VA.id
 ORDER BY VN.name, V.vid;
$$;


ALTER FUNCTION public.vuxml_ranges() OWNER TO dan;

--
-- Name: vuxml_ranges(text); Type: FUNCTION; Schema: public; Owner: dan
--

CREATE FUNCTION public.vuxml_ranges(text) RETURNS SETOF public.vuxml_record
    LANGUAGE sql STABLE
    AS $_$
SELECT V.id,
       V.vid,
       VN.name      AS package_name,
       VR.operator1 AS op1,
       VR.version1  AS v1,
       VR.operator2 AS op2,
       VR.version2  AS v2
  FROM vuxml_ranges VR, vuxml_names VN, vuxml_affected VA, vuxml V
 WHERE V.id                 = VA.vuxml_id
   AND VN.vuxml_affected_id = VA.id
   AND VR.vuxml_affected_id = VA.id
   AND V.vid                = $1
 ORDER BY VN.name, V.vid;
$_$;


ALTER FUNCTION public.vuxml_ranges(text) OWNER TO dan;

--
-- Name: vuxml_ranges_package(text); Type: FUNCTION; Schema: public; Owner: dan
--

CREATE FUNCTION public.vuxml_ranges_package(text) RETURNS SETOF public.vuxml_record
    LANGUAGE sql STABLE
    AS $_$
SELECT V.id,
       V.vid,
       VN.name      AS package_name,
       VR.operator1 AS op1,
       VR.version1  AS v1,
       VR.operator2 AS op2,
       VR.version2  AS v2
  FROM vuxml_ranges VR, vuxml_names VN, vuxml_affected VA, vuxml V
 WHERE VN.name              = $1
   AND V.id                 = VA.vuxml_id
   AND VN.vuxml_affected_id = VA.id
   AND VR.vuxml_affected_id = VA.id
   AND VA.type              = 'package'
 ORDER BY VN.name, V.vid;
$_$;


ALTER FUNCTION public.vuxml_ranges_package(text) OWNER TO dan;

--
-- Name: vuxml_ranges_vid(text); Type: FUNCTION; Schema: public; Owner: dan
--

CREATE FUNCTION public.vuxml_ranges_vid(text) RETURNS SETOF public.vuxml_record
    LANGUAGE sql STABLE
    AS $_$
SELECT V.id,
       V.vid,
       VN.name      AS package_name,
       VR.operator1 AS op1,
       VR.version1  AS v1,
       VR.operator2 AS op2,
       VR.version2  AS v2
  FROM vuxml_ranges VR, vuxml_names VN, vuxml_affected VA, vuxml V
 WHERE V.vid                = $1
   AND V.id                 = VA.vuxml_id
   AND VN.vuxml_affected_id = VA.id
   AND VR.vuxml_affected_id = VA.id
   AND VA.type              = 'package'
 ORDER BY VN.name, V.vid;
$_$;


ALTER FUNCTION public.vuxml_ranges_vid(text) OWNER TO dan;

--
-- Name: watch_list_count(integer); Type: FUNCTION; Schema: public; Owner: dan
--

CREATE FUNCTION public.watch_list_count(integer) RETURNS bigint
    LANGUAGE sql STABLE
    AS $_$
  SELECT count(*) 
    FROM watch_list_element
   WHERE element_id = $1;
$_$;


ALTER FUNCTION public.watch_list_count(integer) OWNER TO dan;

--
-- Name: watch_list_element_delete(); Type: FUNCTION; Schema: public; Owner: dan
--

CREATE FUNCTION public.watch_list_element_delete() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
DECLARE
	l_port_id	int8;
	l_port      text;
	l_category  text;
BEGIN
	RAISE NOTICE 'into watch_list_element_delete with %', OLD.element_id;
	SELECT id, category, name
	  INTO l_port_id, l_category, l_port
     FROM ports_all
    WHERE element_id = OLD.element_id
      AND NOT EXISTS (SELECT CCP.port_id 
                        FROM cache_clearing_ports CCP
                       WHERE CCP.port_id = ports_all.id);
          
   IF FOUND THEN
		RAISE NOTICE 'not found in cache.  will insert %, %, %', l_port_id, l_category, l_port;
		INSERT INTO cache_clearing_ports (port_id, category, port)
          VALUES (l_port_id, l_category, l_port);
		NOTIFY port_updated;
	ELSE
		RAISE NOTICE 'found in cache.  will not insert';
   END IF;

	RETURN OLD;
END; $$;


ALTER FUNCTION public.watch_list_element_delete() OWNER TO dan;

--
-- Name: watch_list_element_insert(); Type: FUNCTION; Schema: public; Owner: dan
--

CREATE FUNCTION public.watch_list_element_insert() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
DECLARE
	l_port_id	int8;
	l_port      text;
	l_category  text;
BEGIN
	RAISE NOTICE 'into watch_list_element_insert with %', NEW.element_id;
	SELECT id, category, name
	  INTO l_port_id, l_category, l_port
     FROM ports_all
    WHERE element_id = NEW.element_id
      AND NOT EXISTS (SELECT CCP.port_id 
                        FROM cache_clearing_ports CCP
                       WHERE CCP.port_id = ports_all.id);
          
   IF FOUND THEN
		RAISE NOTICE 'not found in cache.  will insert %, %, %', l_port_id, l_category, l_port;
		INSERT INTO cache_clearing_ports (port_id, category, port)
          VALUES (l_port_id, l_category, l_port);
		NOTIFY port_updated;
	ELSE
		RAISE NOTICE 'found in cache.  will not insert';
   END IF;

	RETURN new;
END; $$;


ALTER FUNCTION public.watch_list_element_insert() OWNER TO dan;

--
-- Name: watchersalsowatched(integer); Type: FUNCTION; Schema: public; Owner: dan
--

CREATE FUNCTION public.watchersalsowatched(integer) RETURNS SETOF public.also_watched_record
    LANGUAGE sql STABLE
    AS $_$
SELECT O.element_id as element_id, 
       '<a href="/' || P.Category || '/' || P.name || '/" TITLE="' || replace(P.short_description, '"', E'\'') || E'\">' 
                           || P.package_name || '</a>' as url
  FROM ports_active P JOIN (
     SELECT W.element_id
       FROM watch_list_element W
      WHERE w.watch_list_id in (select watch_list_id from watch_list_element where element_id = $1)
        AND W.element_id != $1
   GROUP BY W.element_id
   ORDER BY count(W.watch_list_id) DESC
      LIMIT 5) AS O ON P.element_id = O.element_id;
$_$;


ALTER FUNCTION public.watchersalsowatched(integer) OWNER TO dan;

--
-- Name: watchlistadd(integer, text); Type: FUNCTION; Schema: public; Owner: dan
--

CREATE FUNCTION public.watchlistadd(integer, text) RETURNS boolean
    LANGUAGE plpgsql
    AS $_$
   DECLARE
      WatchListID  ALIAS for $1;
      ElementName  ALIAS for $2;

      EntryExists  boolean = 0;
      ElementID    int4;

   BEGIN
      select Pathname_ID(ElementName)
        into ElementID;

      IF ElementID IS NULL THEN
         RAISE EXCEPTION 'cannot find ElementID for Element = % ', ElementName;
      ELSE
         INSERT INTO Watch_List_Element (watch_list_id, element_id)
                         values (WatchListid,   ElementID);
         EntryExists = 1;
      END IF;

      return EntryExists;
   END;
$_$;


ALTER FUNCTION public.watchlistadd(integer, text) OWNER TO dan;

--
-- Name: watchlistcountdefault(integer); Type: FUNCTION; Schema: public; Owner: dan
--

CREATE FUNCTION public.watchlistcountdefault(integer) RETURNS bigint
    LANGUAGE sql STABLE
    AS $_$
	SELECT count(*)
	  FROM watch_list
     WHERE in_service
	   AND user_id = $1;
$_$;


ALTER FUNCTION public.watchlistcountdefault(integer) OWNER TO dan;

--
-- Name: watchlistdeleteallelements(integer); Type: FUNCTION; Schema: public; Owner: dan
--

CREATE FUNCTION public.watchlistdeleteallelements(integer) RETURNS integer
    LANGUAGE plpgsql
    AS $_$
	DECLARE
		WatchListID	ALIAS for $1;
		RowCount	int;

	BEGIN

		DELETE FROM watch_list_element
		      WHERE watch_list_id  = WatchListID;

		GET DIAGNOSTICS RowCount = ROW_COUNT;

		PERFORM WatchListStagingLogDelete(WatchListID, RowCount);

		RETURN RowCount;
	END
$_$;


ALTER FUNCTION public.watchlistdeleteallelements(integer) OWNER TO dan;

--
-- Name: watchlistdeletedports(bigint); Type: FUNCTION; Schema: public; Owner: dan
--

CREATE FUNCTION public.watchlistdeletedports(bigint) RETURNS SETOF public.old_new_ports_record
    LANGUAGE plpgsql
    AS $_$
DECLARE
	p_WatchListID ALIAS for $1;

	r	 	old_new_ports_record%rowtype;

BEGIN
FOR r IN
SELECT E1.name AS name_old,
       C1.name AS category_old,
       E2.name AS name_new,
       C2.name AS category_new
  FROM watch_list_element WLE,
       ports_moved        PM,
       element            E1,
       ports              P1,
       element            E2,
       ports              P2,
       categories         C1,
       categories         C2
 WHERE WLE.watch_list_id = p_WatchListID
   AND WLE.element_id    = E1.id
   AND E1.id             = P1.element_id
   AND P1.id             = PM.from_port_id
   AND P1.category_id    = C1.id
   AND PM.to_port_id     = P2.id
   AND P2.element_id     = E2.id
   AND P2.category_id    = C2.id
LOOP
	RETURN NEXT r;
END LOOP;
RETURN;
END
$_$;


ALTER FUNCTION public.watchlistdeletedports(bigint) OWNER TO dan;

--
-- Name: watchliststagingadditem(integer, text, text, integer); Type: FUNCTION; Schema: public; Owner: dan
--

CREATE FUNCTION public.watchliststagingadditem(integer, text, text, integer) RETURNS bigint
    LANGUAGE plpgsql
    AS $_$

	DECLARE
		UserID		ALIAS for $1;
		CategoryName	ALIAS for $2;
		PortName	ALIAS for $3;
		Count		ALIAS for $4;

		RC		int8;

	BEGIN
		INSERT INTO watch_list_staging (user_id, category, port, item_count, from_pkg_info, from_watch_list)
		VALUES (UserID, CategoryName, PortName, Count, 'Y', 'N');

		GET DIAGNOSTICS RC = ROW_COUNT;

		RETURN RC;
	END
$_$;


ALTER FUNCTION public.watchliststagingadditem(integer, text, text, integer) OWNER TO dan;

--
-- Name: watchliststagingclear(integer); Type: FUNCTION; Schema: public; Owner: dan
--

CREATE FUNCTION public.watchliststagingclear(integer) RETURNS bigint
    LANGUAGE plpgsql
    AS $_$
   DECLARE
      UserID   ALIAS for $1;
      RowCount int8;

   BEGIN

      DELETE FROM watch_list_staging
            WHERE user_id  = UserID;

      GET DIAGNOSTICS RowCount = ROW_COUNT;

      PERFORM WatchListStagingLogClearing(UserID, RowCount);

      RETURN RowCount;
   END
$_$;


ALTER FUNCTION public.watchliststagingclear(integer) OWNER TO dan;

--
-- Name: watchliststagingclear(bigint); Type: FUNCTION; Schema: public; Owner: dan
--

CREATE FUNCTION public.watchliststagingclear(bigint) RETURNS bigint
    LANGUAGE plpgsql
    AS $_$
	DECLARE
		UserID	ALIAS for $1;
		RowCount	int8;

	BEGIN

		DELETE FROM watch_list_staging
		      WHERE user_id  = UserID;

		GET DIAGNOSTICS RowCount = ROW_COUNT;

		PERFORM WatchListStagingLogClearing(UserID, RowCount);

		RETURN RowCount;
	END
$_$;


ALTER FUNCTION public.watchliststagingclear(bigint) OWNER TO dan;

--
-- Name: watchliststagingexists(integer); Type: FUNCTION; Schema: public; Owner: dan
--

CREATE FUNCTION public.watchliststagingexists(integer) RETURNS integer
    LANGUAGE plpgsql
    AS $_$

-- return TRUE or FALSE if the user is already staging a watch list

	DECLARE
		UserID			ALIAS for $1;
		StagingCount	int4;

	BEGIN
		SELECT count(*)
		  INTO StagingCount
		  FROM watch_list_staging
		 WHERE user_id = UserID;

		IF StagingCount = 0 THEN
			RETURN 0;
		ELSE
			RETURN 1;
		END IF;
	END
$_$;


ALTER FUNCTION public.watchliststagingexists(integer) OWNER TO dan;

--
-- Name: watchliststaginglogclearing(bigint, bigint); Type: FUNCTION; Schema: public; Owner: dan
--

CREATE FUNCTION public.watchliststaginglogclearing(bigint, bigint) RETURNS bigint
    LANGUAGE plpgsql
    AS $_$
	DECLARE
		UserID		ALIAS for $1;
		ItemCount	ALIAS for $2;

		RC		int8;

	BEGIN

		INSERT INTO watch_list_staging_log (	user_id, 
												action,
												count_total)
					VALUES (UserID, 'C', ItemCount);

		GET DIAGNOSTICS RC = ROW_COUNT;

		RETURN RC;
	END
$_$;


ALTER FUNCTION public.watchliststaginglogclearing(bigint, bigint) OWNER TO dan;

--
-- Name: watchliststaginglogdelete(integer, integer); Type: FUNCTION; Schema: public; Owner: dan
--

CREATE FUNCTION public.watchliststaginglogdelete(integer, integer) RETURNS bigint
    LANGUAGE plpgsql
    AS $_$
	DECLARE
		UserID		ALIAS for $1;
		ItemCount	ALIAS for $2;

		RC		int8;

	BEGIN

		INSERT INTO watch_list_staging_log (	user_id, 
												action,
												count_total)
					VALUES (UserID, 'D', ItemCount);

		GET DIAGNOSTICS RC = ROW_COUNT;

		RETURN RC;
	END
$_$;


ALTER FUNCTION public.watchliststaginglogdelete(integer, integer) OWNER TO dan;

--
-- Name: watchliststagingloggeneral(integer, integer, integer, integer, integer, integer); Type: FUNCTION; Schema: public; Owner: dan
--

CREATE FUNCTION public.watchliststagingloggeneral(integer, integer, integer, integer, integer, integer) RETURNS bigint
    LANGUAGE plpgsql
    AS $_$
	DECLARE
		UserID		ALIAS for $1;
		CountTotal	ALIAS for $2;
		CountMatches	ALIAS for $3;
		CountMissing	ALIAS for $4;
		CountDuplicates	ALIAS for $5;
		CountCategories	ALIAS for $6;

		RC		int8;

	BEGIN

		INSERT INTO watch_list_staging_log (	user_id, 
												action, 
												count_total, 
												count_matches, 
												count_missing, 
												count_duplicates, 
												count_categories)
					VALUES (UserID, 'U', CountTotal, CountMatches, 
							CountMissing, CountDuplicates, CountCategories);

		GET DIAGNOSTICS RC = ROW_COUNT;

		RETURN RC;
	END
$_$;


ALTER FUNCTION public.watchliststagingloggeneral(integer, integer, integer, integer, integer, integer) OWNER TO dan;

--
-- Name: watchliststaginglogwatch(integer, integer); Type: FUNCTION; Schema: public; Owner: dan
--

CREATE FUNCTION public.watchliststaginglogwatch(integer, integer) RETURNS bigint
    LANGUAGE plpgsql
    AS $_$
	DECLARE
		UserID		ALIAS for $1;
		CountTotal	ALIAS for $2;

		RC		int8;

	BEGIN

		INSERT INTO watch_list_staging_log (	user_id, 
												action,
												count_total)
					VALUES (UserID, 'W', CountTotal);

		GET DIAGNOSTICS RC = ROW_COUNT;

		RETURN RC;
	END
$_$;


ALTER FUNCTION public.watchliststaginglogwatch(integer, integer) OWNER TO dan;

--
-- Name: watchliststagingprocess(integer); Type: FUNCTION; Schema: public; Owner: dan
--

CREATE FUNCTION public.watchliststagingprocess(integer) RETURNS integer
    LANGUAGE plpgsql
    AS $_$
	DECLARE
		UserID	ALIAS for $1;

		CountTotal			int4;
		CountMatches		int4;
		CountMissing		int4;
		CountDuplicates	int4;
		CountCategories	int4;

	BEGIN
	
-- for each item in watch_list_staging
-- find matching ports and put them 
-- into the table

		UPDATE watch_list_staging SET element_id =
			(	SELECT ports.element_id
				  FROM ports, categories, element
				 WHERE element.name      = watch_list_staging.port
				   AND categories.name   = watch_list_staging.category
				   AND ports.element_id  = element.id
				   AND ports.category_id = categories.id
				   AND element_pathname(ports.element_id) ilike '/ports/head/%')
		 WHERE user_id = UserID;

		SELECT count(*)
		  INTO CountTotal
		  FROM watch_list_staging
		 WHERE user_id = UserID;

		SELECT count(DISTINCT category)
		  INTO CountCategories
		  FROM watch_list_staging
		 WHERE user_id = UserID
		   AND element_id IS NOT NULL;
	
		SELECT count(*)
		  INTO CountMissing
		  FROM watch_list_staging
		 WHERE user_id = UserID
		   AND element_id IS NULL;

		SELECT count(*)
		  INTO CountDuplicates
		  FROM watch_list_staging
		 WHERE user_id = UserID;

		CountMatches := CountTotal - CountMissing;

		PERFORM WatchListStagingLogGeneral(UserID, CountTotal, CountMatches, CountMissing, 
											CountDuplicates, CountCategories);

		RETURN CountTotal;
	END
$_$;


ALTER FUNCTION public.watchliststagingprocess(integer) OWNER TO dan;

--
-- Name: cleanup(); Type: FUNCTION; Schema: session_variables; Owner: dan
--

CREATE FUNCTION session_variables.cleanup() RETURNS void
    LANGUAGE plpgsql
    AS $$

BEGIN

    PERFORM session_variables.create_table();

    EXECUTE 'TRUNCATE _session_variables_data';

    RETURN;

END;

$$;


ALTER FUNCTION session_variables.cleanup() OWNER TO dan;

--
-- Name: create_table(); Type: FUNCTION; Schema: session_variables; Owner: dan
--

CREATE FUNCTION session_variables.create_table() RETURNS void
    LANGUAGE plpgsql
    AS $$

DECLARE

    temprec RECORD;

BEGIN

    LOOP

        SELECT c.relname, n.nspname INTO temprec

            FROM pg_class c join pg_namespace n on c.relnamespace = n.oid

            WHERE c.relkind = 'r' AND c.relname = '_session_variables_data' AND n.nspname ~ '^pg_temp_';

        IF FOUND THEN

            RETURN;

        END IF;

        BEGIN

            EXECUTE 'CREATE TEMP TABLE _session_variables_data(variable_name TEXT PRIMARY KEY, variable_value TEXT, expires_on TIMESTAMPTZ NOT NULL)';

            EXECUTE 'CREATE INDEX session_variables_data_expires_on ON _session_variables_data ( expires_on )';

            RETURN;

        EXCEPTION

            WHEN duplicate_table THEN

                -- ignore, retry loop

        END;

    END LOOP;

END;

$$;


ALTER FUNCTION session_variables.create_table() OWNER TO dan;

--
-- Name: expire(); Type: FUNCTION; Schema: session_variables; Owner: dan
--

CREATE FUNCTION session_variables.expire() RETURNS void
    LANGUAGE plpgsql
    AS $$

BEGIN

    PERFORM session_variables.create_table();

    EXECUTE 'DELETE FROM _session_variables_data WHERE expires_on < ' || quote_literal(clock_timestamp());

    RETURN;

END;

$$;


ALTER FUNCTION session_variables.expire() OWNER TO dan;

--
-- Name: get_value(text); Type: FUNCTION; Schema: session_variables; Owner: dan
--

CREATE FUNCTION session_variables.get_value(_name text) RETURNS text
    LANGUAGE plpgsql
    AS $_$

DECLARE

    reply TEXT;

BEGIN

    PERFORM session_variables.expire();

    EXECUTE 'SELECT variable_value FROM _session_variables_data WHERE variable_name = $1' INTO reply USING _name;

    RETURN reply;

END;

$_$;


ALTER FUNCTION session_variables.get_value(_name text) OWNER TO dan;

--
-- Name: set_value(text, text); Type: FUNCTION; Schema: session_variables; Owner: dan
--

CREATE FUNCTION session_variables.set_value(text, text) RETURNS void
    LANGUAGE sql
    AS $_$

    SELECT session_variables.set_value($1, $2, 'infinity');

$_$;


ALTER FUNCTION session_variables.set_value(text, text) OWNER TO dan;

--
-- Name: set_value(text, text, timestamp with time zone); Type: FUNCTION; Schema: session_variables; Owner: dan
--

CREATE FUNCTION session_variables.set_value(_name text, _value text, _expires timestamp with time zone) RETURNS void
    LANGUAGE plpgsql
    AS $_$

DECLARE

    tempint INT4;

BEGIN

    PERFORM session_variables.expire();

    LOOP

        EXECUTE 'UPDATE _session_variables_data SET variable_value = $2, expires_on = $3 WHERE variable_name = $1' USING _name, _value, _expires;

        GET DIAGNOSTICS tempint = ROW_COUNT;

        IF tempint > 0 THEN

            RETURN;

        END IF;

        BEGIN

            EXECUTE 'INSERT INTO _session_variables_data( variable_name, variable_value, expires_on) VALUES ($1, $2, $3 )' USING _name, _value, _expires;

            RETURN;

        EXCEPTION

            WHEN unique_violation THEN

                -- ignore

        END;

    END LOOP;

END;

$_$;


ALTER FUNCTION session_variables.set_value(_name text, _value text, _expires timestamp with time zone) OWNER TO dan;

--
-- Name: announcements; Type: TABLE; Schema: public; Owner: dan
--

CREATE TABLE public.announcements (
    id integer NOT NULL,
    text text NOT NULL,
    start_date timestamp with time zone,
    end_date timestamp with time zone,
    text_plain text NOT NULL
);


ALTER TABLE public.announcements OWNER TO dan;

--
-- Name: announcements_id_seq; Type: SEQUENCE; Schema: public; Owner: dan
--

CREATE SEQUENCE public.announcements_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.announcements_id_seq OWNER TO dan;

--
-- Name: announcements_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: dan
--

ALTER SEQUENCE public.announcements_id_seq OWNED BY public.announcements.id;


--
-- Name: cache_clearing_dates; Type: TABLE; Schema: public; Owner: dan
--

CREATE TABLE public.cache_clearing_dates (
    id integer NOT NULL,
    date_to_clear date,
    date_added timestamp without time zone DEFAULT now()
);


ALTER TABLE public.cache_clearing_dates OWNER TO dan;

--
-- Name: COLUMN cache_clearing_dates.id; Type: COMMENT; Schema: public; Owner: dan
--

COMMENT ON COLUMN public.cache_clearing_dates.id IS 'primary key';


--
-- Name: COLUMN cache_clearing_dates.date_to_clear; Type: COMMENT; Schema: public; Owner: dan
--

COMMENT ON COLUMN public.cache_clearing_dates.date_to_clear IS 'The date which should be cleared from the archives';


--
-- Name: COLUMN cache_clearing_dates.date_added; Type: COMMENT; Schema: public; Owner: dan
--

COMMENT ON COLUMN public.cache_clearing_dates.date_added IS 'The timestamp this entry was added to the table';


--
-- Name: cache_clearing_dates_id_seq; Type: SEQUENCE; Schema: public; Owner: dan
--

CREATE SEQUENCE public.cache_clearing_dates_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.cache_clearing_dates_id_seq OWNER TO dan;

--
-- Name: cache_clearing_dates_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: dan
--

ALTER SEQUENCE public.cache_clearing_dates_id_seq OWNED BY public.cache_clearing_dates.id;


--
-- Name: cache_clearing_ports; Type: TABLE; Schema: public; Owner: dan
--

CREATE TABLE public.cache_clearing_ports (
    id integer NOT NULL,
    port_id integer NOT NULL,
    category text NOT NULL,
    port text NOT NULL,
    date_added timestamp without time zone DEFAULT now() NOT NULL
);


ALTER TABLE public.cache_clearing_ports OWNER TO dan;

--
-- Name: cache_clearing_ports_id_seq; Type: SEQUENCE; Schema: public; Owner: dan
--

CREATE SEQUENCE public.cache_clearing_ports_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.cache_clearing_ports_id_seq OWNER TO dan;

--
-- Name: cache_clearing_ports_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: dan
--

ALTER SEQUENCE public.cache_clearing_ports_id_seq OWNED BY public.cache_clearing_ports.id;


--
-- Name: categories; Type: TABLE; Schema: public; Owner: dan
--

CREATE TABLE public.categories (
    id integer NOT NULL,
    is_primary boolean NOT NULL,
    element_id integer,
    name text NOT NULL,
    description text
);


ALTER TABLE public.categories OWNER TO dan;

--
-- Name: categories_id_seq; Type: SEQUENCE; Schema: public; Owner: dan
--

CREATE SEQUENCE public.categories_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.categories_id_seq OWNER TO dan;

--
-- Name: categories_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: dan
--

ALTER SEQUENCE public.categories_id_seq OWNED BY public.categories.id;


--
-- Name: category_stats; Type: TABLE; Schema: public; Owner: dan
--

CREATE TABLE public.category_stats (
    category_id integer NOT NULL,
    port_count integer NOT NULL,
    last_update timestamp without time zone NOT NULL
);


ALTER TABLE public.category_stats OWNER TO dan;

--
-- Name: commit_log; Type: TABLE; Schema: public; Owner: dan
--

CREATE TABLE public.commit_log (
    id integer NOT NULL,
    message_id text NOT NULL,
    message_date timestamp with time zone NOT NULL,
    message_subject text,
    date_added timestamp with time zone NOT NULL,
    commit_date timestamp with time zone NOT NULL,
    committer text NOT NULL,
    description text NOT NULL,
    system_id integer NOT NULL,
    encoding_losses boolean DEFAULT false NOT NULL,
    svn_revision text,
    repo_id integer
);


ALTER TABLE public.commit_log OWNER TO dan;

--
-- Name: commit_log_branch_delete_me; Type: TABLE; Schema: public; Owner: dan
--

CREATE TABLE public.commit_log_branch_delete_me (
    commit_log_id integer NOT NULL,
    branch_id integer NOT NULL
);


ALTER TABLE public.commit_log_branch_delete_me OWNER TO dan;

--
-- Name: commit_log_branches; Type: TABLE; Schema: public; Owner: dan
--

CREATE TABLE public.commit_log_branches (
    commit_log_id integer NOT NULL,
    branch_id integer NOT NULL
);


ALTER TABLE public.commit_log_branches OWNER TO dan;

--
-- Name: commit_log_elements; Type: TABLE; Schema: public; Owner: dan
--

CREATE TABLE public.commit_log_elements (
    id integer NOT NULL,
    commit_log_id integer NOT NULL,
    element_id integer NOT NULL,
    revision_name text,
    change_type character(1) NOT NULL,
    CONSTRAINT commit_log_elements_change_type CHECK (((change_type = 'A'::bpchar) OR (change_type = 'M'::bpchar) OR (change_type = 'R'::bpchar)))
);


ALTER TABLE public.commit_log_elements OWNER TO dan;

--
-- Name: commit_log_elements_id_seq; Type: SEQUENCE; Schema: public; Owner: dan
--

CREATE SEQUENCE public.commit_log_elements_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.commit_log_elements_id_seq OWNER TO dan;

--
-- Name: commit_log_elements_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: dan
--

ALTER SEQUENCE public.commit_log_elements_id_seq OWNED BY public.commit_log_elements.id;


--
-- Name: commit_log_id_seq; Type: SEQUENCE; Schema: public; Owner: dan
--

CREATE SEQUENCE public.commit_log_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.commit_log_id_seq OWNER TO dan;

--
-- Name: commit_log_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: dan
--

ALTER SEQUENCE public.commit_log_id_seq OWNED BY public.commit_log.id;


--
-- Name: commit_log_port_elements; Type: TABLE; Schema: public; Owner: dan
--

CREATE TABLE public.commit_log_port_elements (
    commit_log_id integer NOT NULL,
    port_id integer NOT NULL,
    commit_log_element_id integer NOT NULL
);


ALTER TABLE public.commit_log_port_elements OWNER TO dan;

--
-- Name: commit_log_ports; Type: TABLE; Schema: public; Owner: dan
--

CREATE TABLE public.commit_log_ports (
    commit_log_id integer NOT NULL,
    port_id integer NOT NULL,
    needs_refresh smallint NOT NULL,
    port_version text,
    port_revision text,
    port_epoch text,
    port_name_revision text
);


ALTER TABLE public.commit_log_ports OWNER TO dan;

--
-- Name: commit_log_ports_elements; Type: TABLE; Schema: public; Owner: dan
--

CREATE TABLE public.commit_log_ports_elements (
    commit_log_id integer NOT NULL,
    element_id integer NOT NULL
);


ALTER TABLE public.commit_log_ports_elements OWNER TO dan;

--
-- Name: commit_log_ports_ignore; Type: TABLE; Schema: public; Owner: dan
--

CREATE TABLE public.commit_log_ports_ignore (
    commit_log_id integer NOT NULL,
    port_id integer NOT NULL,
    date_ignored timestamp with time zone DEFAULT ('now'::text)::timestamp(6) with time zone NOT NULL,
    reason text NOT NULL
);


ALTER TABLE public.commit_log_ports_ignore OWNER TO dan;

--
-- Name: commit_log_ports_vuxml; Type: TABLE; Schema: public; Owner: dan
--

CREATE TABLE public.commit_log_ports_vuxml (
    id integer NOT NULL,
    commit_log_id integer NOT NULL,
    port_id integer NOT NULL,
    vuxml_id integer NOT NULL
);
ALTER TABLE ONLY public.commit_log_ports_vuxml ALTER COLUMN port_id SET STATISTICS 1000;
ALTER TABLE ONLY public.commit_log_ports_vuxml ALTER COLUMN vuxml_id SET STATISTICS 1000;


ALTER TABLE public.commit_log_ports_vuxml OWNER TO dan;

--
-- Name: commit_log_ports_vuxml_id_seq; Type: SEQUENCE; Schema: public; Owner: dan
--

CREATE SEQUENCE public.commit_log_ports_vuxml_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.commit_log_ports_vuxml_id_seq OWNER TO dan;

--
-- Name: commit_log_ports_vuxml_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: dan
--

ALTER SEQUENCE public.commit_log_ports_vuxml_id_seq OWNED BY public.commit_log_ports_vuxml.id;


--
-- Name: commits_flagged; Type: TABLE; Schema: public; Owner: dan
--

CREATE TABLE public.commits_flagged (
    user_id integer NOT NULL,
    commit_log_id integer NOT NULL
);


ALTER TABLE public.commits_flagged OWNER TO dan;

--
-- Name: commits_latest; Type: TABLE; Schema: public; Owner: dan
--

CREATE TABLE public.commits_latest (
    commit_log_id integer,
    commit_date_raw timestamp with time zone,
    message_subject text,
    message_id text,
    committer text,
    commit_description text,
    commit_date text,
    commit_time text,
    element_id integer,
    element_name text,
    revision_name text,
    status character(1),
    encoding_losses boolean,
    element_pathname text
);


ALTER TABLE public.commits_latest OWNER TO dan;

--
-- Name: commits_latest_ports; Type: TABLE; Schema: public; Owner: dan
--

CREATE TABLE public.commits_latest_ports (
    commit_log_id integer NOT NULL,
    commit_date timestamp without time zone NOT NULL
);


ALTER TABLE public.commits_latest_ports OWNER TO dan;

--
-- Name: commits_recent; Type: VIEW; Schema: public; Owner: dan
--

CREATE VIEW public.commits_recent AS
 SELECT commit_log.id,
    commit_log.message_id,
    commit_log.message_date,
    commit_log.message_subject,
    commit_log.date_added,
    commit_log.commit_date,
    commit_log.committer,
    commit_log.description,
    commit_log.system_id,
    commit_log.encoding_losses
   FROM public.commit_log
  ORDER BY commit_log.commit_date DESC, commit_log.id
 LIMIT 100;


ALTER TABLE public.commits_recent OWNER TO dan;

--
-- Name: commits_recent_ports; Type: VIEW; Schema: public; Owner: dan
--

CREATE VIEW public.commits_recent_ports AS
 SELECT DISTINCT commit_log.id,
    commit_log.message_id,
    commit_log.message_date,
    commit_log.message_subject,
    commit_log.date_added,
    commit_log.commit_date,
    commit_log.committer,
    commit_log.description,
    commit_log.system_id,
    commit_log.encoding_losses
   FROM public.commit_log
  WHERE (EXISTS ( SELECT commit_log_ports.commit_log_id,
            commit_log_ports.port_id,
            commit_log_ports.needs_refresh,
            commit_log_ports.port_version,
            commit_log_ports.port_revision
           FROM public.commit_log_ports
          WHERE (commit_log_ports.commit_log_id = commit_log.id)))
  ORDER BY commit_log.commit_date DESC, commit_log.id, commit_log.message_id, commit_log.message_date, commit_log.message_subject, commit_log.date_added, commit_log.committer, commit_log.description, commit_log.system_id, commit_log.encoding_losses
 LIMIT 100;


ALTER TABLE public.commits_recent_ports OWNER TO dan;

--
-- Name: committer_notify; Type: TABLE; Schema: public; Owner: dan
--

CREATE TABLE public.committer_notify (
    user_id integer NOT NULL,
    committer text NOT NULL,
    status character(1) NOT NULL
);


ALTER TABLE public.committer_notify OWNER TO dan;

--
-- Name: daily_refreshes; Type: TABLE; Schema: public; Owner: dan
--

CREATE TABLE public.daily_refreshes (
    refresh_date date NOT NULL
);


ALTER TABLE public.daily_refreshes OWNER TO dan;

--
-- Name: daily_stats; Type: TABLE; Schema: public; Owner: dan
--

CREATE TABLE public.daily_stats (
    id integer DEFAULT nextval(('daily_stats_seq'::text)::regclass) NOT NULL,
    title text,
    query text
);


ALTER TABLE public.daily_stats OWNER TO dan;

--
-- Name: daily_stats_data; Type: TABLE; Schema: public; Owner: dan
--

CREATE TABLE public.daily_stats_data (
    id integer DEFAULT nextval(('daily_stats_data_seq'::text)::regclass) NOT NULL,
    daily_stats_id integer NOT NULL,
    date date NOT NULL,
    value bigint
);


ALTER TABLE public.daily_stats_data OWNER TO dan;

--
-- Name: daily_stats_data_seq; Type: SEQUENCE; Schema: public; Owner: dan
--

CREATE SEQUENCE public.daily_stats_data_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.daily_stats_data_seq OWNER TO dan;

--
-- Name: daily_stats_seq; Type: SEQUENCE; Schema: public; Owner: dan
--

CREATE SEQUENCE public.daily_stats_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.daily_stats_seq OWNER TO dan;

--
-- Name: design_results; Type: TABLE; Schema: public; Owner: dan
--

CREATE TABLE public.design_results (
    id integer NOT NULL,
    user_id integer NOT NULL,
    choice1 character(1) NOT NULL,
    choice2 character(1) NOT NULL,
    choice3 character(1) NOT NULL,
    CONSTRAINT choice_all_different CHECK (((choice1 <> choice2) AND (choice1 <> choice3) AND (choice2 <> choice3))),
    CONSTRAINT valid_choices CHECK (((choice1 = ANY (ARRAY['A'::bpchar, 'B'::bpchar, 'C'::bpchar])) AND (choice2 = ANY (ARRAY['A'::bpchar, 'B'::bpchar, 'C'::bpchar])) AND (choice3 = ANY (ARRAY['A'::bpchar, 'B'::bpchar, 'C'::bpchar]))))
);


ALTER TABLE public.design_results OWNER TO dan;

--
-- Name: design_results_id_seq; Type: SEQUENCE; Schema: public; Owner: dan
--

CREATE SEQUENCE public.design_results_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.design_results_id_seq OWNER TO dan;

--
-- Name: design_results_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: dan
--

ALTER SEQUENCE public.design_results_id_seq OWNED BY public.design_results.id;


--
-- Name: element; Type: TABLE; Schema: public; Owner: dan
--

CREATE TABLE public.element (
    id integer NOT NULL,
    name text NOT NULL,
    parent_id integer,
    directory_file_flag character(1) NOT NULL,
    status character(1) NOT NULL,
    CONSTRAINT element_directory_file_flag CHECK (((directory_file_flag = 'F'::bpchar) OR (directory_file_flag = 'D'::bpchar))),
    CONSTRAINT element_status CHECK (((status = 'A'::bpchar) OR (status = 'D'::bpchar)))
);


ALTER TABLE public.element OWNER TO dan;

--
-- Name: element_id_seq; Type: SEQUENCE; Schema: public; Owner: dan
--

CREATE SEQUENCE public.element_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.element_id_seq OWNER TO dan;

--
-- Name: element_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: dan
--

ALTER SEQUENCE public.element_id_seq OWNED BY public.element.id;


--
-- Name: element_pathname; Type: TABLE; Schema: public; Owner: dan
--

CREATE TABLE public.element_pathname (
    element_id integer NOT NULL,
    pathname text NOT NULL
);


ALTER TABLE public.element_pathname OWNER TO dan;

--
-- Name: element_revision; Type: TABLE; Schema: public; Owner: dan
--

CREATE TABLE public.element_revision (
    element_id integer NOT NULL,
    revision_name text NOT NULL
);


ALTER TABLE public.element_revision OWNER TO dan;

--
-- Name: flavors_id_seq; Type: SEQUENCE; Schema: public; Owner: dan
--

CREATE SEQUENCE public.flavors_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.flavors_id_seq OWNER TO dan;

--
-- Name: flavors; Type: TABLE; Schema: public; Owner: dan
--

CREATE TABLE public.flavors (
    id bigint DEFAULT nextval('public.flavors_id_seq'::regclass) NOT NULL,
    name text NOT NULL
);


ALTER TABLE public.flavors OWNER TO dan;

--
-- Name: generate_plist_id_seq; Type: SEQUENCE; Schema: public; Owner: dan
--

CREATE SEQUENCE public.generate_plist_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.generate_plist_id_seq OWNER TO dan;

--
-- Name: generate_plist; Type: TABLE; Schema: public; Owner: dan
--

CREATE TABLE public.generate_plist (
    id bigint DEFAULT nextval('public.generate_plist_id_seq'::regclass) NOT NULL,
    port_id integer NOT NULL,
    installed_file text NOT NULL
);


ALTER TABLE public.generate_plist OWNER TO dan;

--
-- Name: graphs; Type: TABLE; Schema: public; Owner: dan
--

CREATE TABLE public.graphs (
    id integer NOT NULL,
    title text NOT NULL,
    query text NOT NULL,
    label text,
    is_clickable boolean,
    json boolean DEFAULT false
);


ALTER TABLE public.graphs OWNER TO dan;

--
-- Name: graphs_id_seq; Type: SEQUENCE; Schema: public; Owner: dan
--

CREATE SEQUENCE public.graphs_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.graphs_id_seq OWNER TO dan;

--
-- Name: graphs_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: dan
--

ALTER SEQUENCE public.graphs_id_seq OWNED BY public.graphs.id;


--
-- Name: latest_commits; Type: TABLE; Schema: public; Owner: dan
--

CREATE TABLE public.latest_commits (
    commit_log_id integer NOT NULL,
    commit_date timestamp with time zone NOT NULL
);


ALTER TABLE public.latest_commits OWNER TO dan;

--
-- Name: latest_commits_ports; Type: TABLE; Schema: public; Owner: dan
--

CREATE TABLE public.latest_commits_ports (
    commit_log_id integer NOT NULL,
    commit_date timestamp with time zone NOT NULL
);


ALTER TABLE public.latest_commits_ports OWNER TO dan;

--
-- Name: listen_for; Type: TABLE; Schema: public; Owner: dan
--

CREATE TABLE public.listen_for (
    id integer NOT NULL,
    name text NOT NULL,
    script_name text NOT NULL
);


ALTER TABLE public.listen_for OWNER TO dan;

--
-- Name: listen_for_id_seq; Type: SEQUENCE; Schema: public; Owner: dan
--

CREATE SEQUENCE public.listen_for_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.listen_for_id_seq OWNER TO dan;

--
-- Name: listen_for_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: dan
--

ALTER SEQUENCE public.listen_for_id_seq OWNED BY public.listen_for.id;


--
-- Name: maxcommitid; Type: TABLE; Schema: public; Owner: dan
--

CREATE TABLE public.maxcommitid (
    max integer
);


ALTER TABLE public.maxcommitid OWNER TO dan;

--
-- Name: package_flavors_id_seq; Type: SEQUENCE; Schema: public; Owner: dan
--

CREATE SEQUENCE public.package_flavors_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.package_flavors_id_seq OWNER TO dan;

--
-- Name: package_flavors; Type: TABLE; Schema: public; Owner: dan
--

CREATE TABLE public.package_flavors (
    id bigint DEFAULT nextval('public.package_flavors_id_seq'::regclass) NOT NULL,
    port_id bigint NOT NULL,
    flavor_id bigint NOT NULL,
    name text NOT NULL,
    flavor_number integer NOT NULL
);


ALTER TABLE public.package_flavors OWNER TO dan;

--
-- Name: page_load_detail; Type: TABLE; Schema: public; Owner: dan
--

CREATE TABLE public.page_load_detail (
    id integer NOT NULL,
    date date DEFAULT ('now'::text)::date NOT NULL,
    "time" time without time zone DEFAULT ('now'::text)::time(6) without time zone NOT NULL,
    page_name text NOT NULL,
    user_id integer,
    ip_address inet NOT NULL,
    full_url text NOT NULL,
    rendering_time interval NOT NULL
);
ALTER TABLE ONLY public.page_load_detail ALTER COLUMN date SET STATISTICS 1000;


ALTER TABLE public.page_load_detail OWNER TO dan;

--
-- Name: page_load_detail_id_seq; Type: SEQUENCE; Schema: public; Owner: dan
--

CREATE SEQUENCE public.page_load_detail_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.page_load_detail_id_seq OWNER TO dan;

--
-- Name: page_load_detail_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: dan
--

ALTER SEQUENCE public.page_load_detail_id_seq OWNED BY public.page_load_detail.id;


--
-- Name: page_load_summary; Type: TABLE; Schema: public; Owner: dan
--

CREATE TABLE public.page_load_summary (
    id integer NOT NULL,
    date date NOT NULL,
    page_name text NOT NULL,
    total integer NOT NULL,
    users integer NOT NULL,
    rendering_time_min interval NOT NULL,
    rendering_time_max interval NOT NULL,
    rendering_time_avg interval NOT NULL
);


ALTER TABLE public.page_load_summary OWNER TO dan;

--
-- Name: page_load_summary_id_seq; Type: SEQUENCE; Schema: public; Owner: dan
--

CREATE SEQUENCE public.page_load_summary_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.page_load_summary_id_seq OWNER TO dan;

--
-- Name: page_load_summary_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: dan
--

ALTER SEQUENCE public.page_load_summary_id_seq OWNED BY public.page_load_summary.id;


--
-- Name: port_dependencies; Type: TABLE; Schema: public; Owner: dan
--

CREATE TABLE public.port_dependencies (
    port_id integer NOT NULL,
    port_id_dependent_upon integer NOT NULL,
    dependency_type character(1) NOT NULL
);


ALTER TABLE public.port_dependencies OWNER TO dan;

--
-- Name: portcount; Type: TABLE; Schema: public; Owner: dan
--

CREATE TABLE public.portcount (
    count bigint
);


ALTER TABLE public.portcount OWNER TO dan;

--
-- Name: ports; Type: TABLE; Schema: public; Owner: dan
--

CREATE TABLE public.ports (
    id integer NOT NULL,
    element_id integer NOT NULL,
    category_id integer NOT NULL,
    short_description text,
    long_description text,
    version text,
    revision text,
    maintainer text,
    homepage text,
    master_sites text,
    extract_suffix text,
    package_exists boolean,
    depends_build text,
    depends_run text,
    last_commit_id integer,
    found_in_index boolean,
    forbidden text,
    broken text,
    date_added timestamp with time zone DEFAULT ('now'::text)::timestamp(6) with time zone,
    categories text,
    deprecated text,
    ignore text,
    master_port text,
    latest_link text,
    depends_lib text,
    no_latest_link text,
    no_package text,
    package_name text,
    portepoch text,
    no_cdrom text,
    restricted text,
    expiration_date date,
    is_interactive text,
    only_for_archs text,
    not_for_archs text,
    status character(1) NOT NULL,
    showconfig text,
    license text,
    fetch_depends text,
    extract_depends text,
    patch_depends text,
    uses text,
    pkgmessage text,
    distinfo text,
    license_restricted text,
    manual_package_build text,
    license_perms text,
    pkg_plist text,
    makefile text,
    conflicts text,
    conflicts_build text,
    conflicts_install text
);


ALTER TABLE public.ports OWNER TO dan;

--
-- Name: ports_active; Type: VIEW; Schema: public; Owner: dan
--

CREATE VIEW public.ports_active AS
 SELECT ports.id,
    ports.element_id,
    ports.category_id,
    ports.short_description,
    ports.long_description,
    ports.version,
    ports.revision,
    ports.maintainer,
    ports.homepage,
    ports.master_sites,
    ports.extract_suffix,
    ports.package_exists,
    ports.depends_build,
    ports.depends_run,
    ports.last_commit_id,
    ports.found_in_index,
    ports.forbidden,
    ports.broken,
    ports.date_added,
    ports.categories,
    ports.deprecated,
    ports.ignore,
    ports.master_port,
    ports.latest_link,
    ports.depends_lib,
    ports.no_latest_link,
    ports.no_package,
    ports.package_name,
    ports.portepoch,
    ports.no_cdrom,
    ports.restricted,
    ports.expiration_date,
    ports.is_interactive,
    ports.only_for_archs,
    ports.not_for_archs,
    ports.status,
    ports.showconfig,
    ports.license,
    ports.fetch_depends,
    ports.extract_depends,
    ports.patch_depends,
    ports.uses,
    ports.pkgmessage,
    ports.distinfo,
    ports.license_restricted,
    ports.manual_package_build,
    ports.license_perms,
    element.name,
    categories.name AS category
   FROM public.categories,
    public.ports,
    public.element
  WHERE ((element.status = 'A'::bpchar) AND (categories.id = ports.category_id) AND (ports.element_id = element.id));


ALTER TABLE public.ports_active OWNER TO dan;

--
-- Name: ports_all; Type: VIEW; Schema: public; Owner: dan
--

CREATE VIEW public.ports_all AS
 SELECT ports.id,
    ports.element_id,
    ports.category_id,
    ports.short_description,
    ports.long_description,
    ports.version,
    ports.revision,
    ports.maintainer,
    ports.homepage,
    ports.master_sites,
    ports.extract_suffix,
    ports.package_exists,
    ports.depends_build,
    ports.depends_run,
    ports.last_commit_id,
    ports.found_in_index,
    ports.forbidden,
    ports.broken,
    ports.date_added,
    ports.categories,
    ports.deprecated,
    ports.ignore,
    ports.master_port,
    ports.latest_link,
    ports.depends_lib,
    ports.no_latest_link,
    ports.no_package,
    ports.package_name,
    ports.portepoch,
    ports.no_cdrom,
    ports.restricted,
    ports.expiration_date,
    ports.is_interactive,
    ports.only_for_archs,
    ports.not_for_archs,
    ports.status,
    ports.showconfig,
    ports.license,
    ports.fetch_depends,
    ports.extract_depends,
    ports.patch_depends,
    ports.uses,
    ports.pkgmessage,
    ports.distinfo,
    ports.license_restricted,
    ports.manual_package_build,
    ports.license_perms,
    ports.pkg_plist,
    ports.makefile,
    element.name,
    categories.name AS category
   FROM public.categories,
    public.ports,
    public.element
  WHERE ((categories.id = ports.category_id) AND (ports.element_id = element.id));


ALTER TABLE public.ports_all OWNER TO dan;

--
-- Name: ports_categories; Type: TABLE; Schema: public; Owner: dan
--

CREATE TABLE public.ports_categories (
    port_id integer NOT NULL,
    category_id integer NOT NULL
);


ALTER TABLE public.ports_categories OWNER TO dan;

--
-- Name: ports_check; Type: TABLE; Schema: public; Owner: dan
--

CREATE TABLE public.ports_check (
    id integer NOT NULL,
    category_name text NOT NULL,
    port_name text NOT NULL,
    category_id integer,
    port_id integer,
    add_to_ports_table boolean DEFAULT true NOT NULL
);


ALTER TABLE public.ports_check OWNER TO dan;

--
-- Name: ports_check_id_seq; Type: SEQUENCE; Schema: public; Owner: dan
--

CREATE SEQUENCE public.ports_check_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.ports_check_id_seq OWNER TO dan;

--
-- Name: ports_check_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: dan
--

ALTER SEQUENCE public.ports_check_id_seq OWNED BY public.ports_check.id;


--
-- Name: ports_conflicts; Type: TABLE; Schema: public; Owner: dan
--

CREATE TABLE public.ports_conflicts (
    id bigint NOT NULL,
    port_id integer NOT NULL,
    conflicts_type public.conflicts NOT NULL,
    item_num integer NOT NULL,
    item_value text NOT NULL
);


ALTER TABLE public.ports_conflicts OWNER TO dan;

--
-- Name: ports_conflicts_id_seq; Type: SEQUENCE; Schema: public; Owner: dan
--

CREATE SEQUENCE public.ports_conflicts_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.ports_conflicts_id_seq OWNER TO dan;

--
-- Name: ports_conflicts_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: dan
--

ALTER SEQUENCE public.ports_conflicts_id_seq OWNED BY public.ports_conflicts.id;


--
-- Name: ports_id_seq; Type: SEQUENCE; Schema: public; Owner: dan
--

CREATE SEQUENCE public.ports_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.ports_id_seq OWNER TO dan;

--
-- Name: ports_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: dan
--

ALTER SEQUENCE public.ports_id_seq OWNED BY public.ports.id;


--
-- Name: ports_moved; Type: TABLE; Schema: public; Owner: dan
--

CREATE TABLE public.ports_moved (
    id integer NOT NULL,
    from_port_id integer NOT NULL,
    to_port_id integer,
    date date NOT NULL,
    reason text NOT NULL
);


ALTER TABLE public.ports_moved OWNER TO dan;

--
-- Name: ports_moved_id_seq; Type: SEQUENCE; Schema: public; Owner: dan
--

CREATE SEQUENCE public.ports_moved_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.ports_moved_id_seq OWNER TO dan;

--
-- Name: ports_moved_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: dan
--

ALTER SEQUENCE public.ports_moved_id_seq OWNED BY public.ports_moved.id;


--
-- Name: ports_updating; Type: TABLE; Schema: public; Owner: dan
--

CREATE TABLE public.ports_updating (
    id integer NOT NULL,
    date date NOT NULL,
    affects text NOT NULL,
    author text,
    reason text NOT NULL
);


ALTER TABLE public.ports_updating OWNER TO dan;

--
-- Name: ports_updating_id_seq; Type: SEQUENCE; Schema: public; Owner: dan
--

CREATE SEQUENCE public.ports_updating_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.ports_updating_id_seq OWNER TO dan;

--
-- Name: ports_updating_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: dan
--

ALTER SEQUENCE public.ports_updating_id_seq OWNED BY public.ports_updating.id;


--
-- Name: ports_updating_ports_xref; Type: TABLE; Schema: public; Owner: dan
--

CREATE TABLE public.ports_updating_ports_xref (
    ports_updating_id integer NOT NULL,
    port_id integer NOT NULL
);


ALTER TABLE public.ports_updating_ports_xref OWNER TO dan;

--
-- Name: ports_vulnerable; Type: TABLE; Schema: public; Owner: dan
--

CREATE TABLE public.ports_vulnerable (
    port_id integer NOT NULL,
    current integer DEFAULT 1 NOT NULL,
    past integer DEFAULT 0 NOT NULL
);


ALTER TABLE public.ports_vulnerable OWNER TO dan;

--
-- Name: repo; Type: TABLE; Schema: public; Owner: dan
--

CREATE TABLE public.repo (
    id integer NOT NULL,
    name text NOT NULL,
    description text NOT NULL,
    svn_hostname text NOT NULL,
    path_to_repo text NOT NULL
);


ALTER TABLE public.repo OWNER TO dan;

--
-- Name: repo_id_seq; Type: SEQUENCE; Schema: public; Owner: dan
--

CREATE SEQUENCE public.repo_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.repo_id_seq OWNER TO dan;

--
-- Name: repo_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: dan
--

ALTER SEQUENCE public.repo_id_seq OWNED BY public.repo.id;


--
-- Name: report_frequency; Type: TABLE; Schema: public; Owner: dan
--

CREATE TABLE public.report_frequency (
    id integer NOT NULL,
    frequency character(1) NOT NULL,
    description text NOT NULL
);


ALTER TABLE public.report_frequency OWNER TO dan;

--
-- Name: report_frequency_id_seq; Type: SEQUENCE; Schema: public; Owner: dan
--

CREATE SEQUENCE public.report_frequency_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.report_frequency_id_seq OWNER TO dan;

--
-- Name: report_frequency_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: dan
--

ALTER SEQUENCE public.report_frequency_id_seq OWNED BY public.report_frequency.id;


--
-- Name: report_log; Type: TABLE; Schema: public; Owner: dan
--

CREATE TABLE public.report_log (
    id integer NOT NULL,
    report_id integer NOT NULL,
    frequency_id integer,
    report_date timestamp with time zone DEFAULT ('now'::text)::timestamp(6) with time zone NOT NULL,
    email_count integer NOT NULL,
    commit_count integer NOT NULL,
    port_count integer NOT NULL
);


ALTER TABLE public.report_log OWNER TO dan;

--
-- Name: report_log_id_seq; Type: SEQUENCE; Schema: public; Owner: dan
--

CREATE SEQUENCE public.report_log_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.report_log_id_seq OWNER TO dan;

--
-- Name: report_log_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: dan
--

ALTER SEQUENCE public.report_log_id_seq OWNED BY public.report_log.id;


--
-- Name: report_log_latest; Type: VIEW; Schema: public; Owner: dan
--

CREATE VIEW public.report_log_latest AS
 SELECT report_log.report_id,
    report_log.frequency_id,
    report_frequency.frequency,
    max(report_log.report_date) AS last_sent
   FROM public.report_log,
    public.report_frequency
  WHERE (report_log.frequency_id = report_frequency.id)
  GROUP BY report_log.report_id, report_log.frequency_id, report_frequency.frequency;


ALTER TABLE public.report_log_latest OWNER TO dan;

--
-- Name: report_subscriptions; Type: TABLE; Schema: public; Owner: dan
--

CREATE TABLE public.report_subscriptions (
    report_id integer NOT NULL,
    user_id integer NOT NULL,
    report_frequency_id integer
);


ALTER TABLE public.report_subscriptions OWNER TO dan;

--
-- Name: reports; Type: TABLE; Schema: public; Owner: dan
--

CREATE TABLE public.reports (
    id integer NOT NULL,
    name text NOT NULL,
    description text NOT NULL,
    needs_frequency boolean,
    CONSTRAINT needs_frequency CHECK ((needs_frequency IS NOT NULL))
);


ALTER TABLE public.reports OWNER TO dan;

--
-- Name: reports_id_seq; Type: SEQUENCE; Schema: public; Owner: dan
--

CREATE SEQUENCE public.reports_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.reports_id_seq OWNER TO dan;

--
-- Name: reports_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: dan
--

ALTER SEQUENCE public.reports_id_seq OWNED BY public.reports.id;


--
-- Name: sanity_test_failures; Type: TABLE; Schema: public; Owner: dan
--

CREATE TABLE public.sanity_test_failures (
    id integer NOT NULL,
    commit_log_id integer NOT NULL,
    message text NOT NULL
);


ALTER TABLE public.sanity_test_failures OWNER TO dan;

--
-- Name: sanity_test_failures_id_seq; Type: SEQUENCE; Schema: public; Owner: dan
--

CREATE SEQUENCE public.sanity_test_failures_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.sanity_test_failures_id_seq OWNER TO dan;

--
-- Name: sanity_test_failures_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: dan
--

ALTER SEQUENCE public.sanity_test_failures_id_seq OWNED BY public.sanity_test_failures.id;


--
-- Name: security_notice; Type: TABLE; Schema: public; Owner: dan
--

CREATE TABLE public.security_notice (
    id integer NOT NULL,
    user_id integer NOT NULL,
    date_added timestamp with time zone DEFAULT ('now'::text)::timestamp(6) with time zone NOT NULL,
    ip_address inet NOT NULL,
    description text NOT NULL,
    commit_log_id integer NOT NULL,
    security_notice_status_id character(1) DEFAULT 'A'::bpchar NOT NULL
);


ALTER TABLE public.security_notice OWNER TO dan;

--
-- Name: security_notice_audit; Type: TABLE; Schema: public; Owner: dan
--

CREATE TABLE public.security_notice_audit (
    id integer NOT NULL,
    security_notice_id integer NOT NULL,
    user_id integer NOT NULL,
    date_added timestamp with time zone DEFAULT ('now'::text)::timestamp(6) with time zone NOT NULL,
    ip_address inet NOT NULL,
    description text NOT NULL,
    commit_log_id integer NOT NULL,
    security_notice_status_id character(1) DEFAULT 'A'::bpchar NOT NULL
);


ALTER TABLE public.security_notice_audit OWNER TO dan;

--
-- Name: security_notice_audit_id_seq; Type: SEQUENCE; Schema: public; Owner: dan
--

CREATE SEQUENCE public.security_notice_audit_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.security_notice_audit_id_seq OWNER TO dan;

--
-- Name: security_notice_audit_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: dan
--

ALTER SEQUENCE public.security_notice_audit_id_seq OWNED BY public.security_notice_audit.id;


--
-- Name: security_notice_id_seq; Type: SEQUENCE; Schema: public; Owner: dan
--

CREATE SEQUENCE public.security_notice_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.security_notice_id_seq OWNER TO dan;

--
-- Name: security_notice_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: dan
--

ALTER SEQUENCE public.security_notice_id_seq OWNED BY public.security_notice.id;


--
-- Name: security_notice_status; Type: TABLE; Schema: public; Owner: dan
--

CREATE TABLE public.security_notice_status (
    id character(1) NOT NULL,
    name text NOT NULL,
    description text NOT NULL
);


ALTER TABLE public.security_notice_status OWNER TO dan;

--
-- Name: system; Type: TABLE; Schema: public; Owner: dan
--

CREATE TABLE public.system (
    id integer NOT NULL,
    name text NOT NULL,
    time_adjust interval DEFAULT '00:00:00'::interval NOT NULL
);


ALTER TABLE public.system OWNER TO dan;

--
-- Name: system_branch; Type: TABLE; Schema: public; Owner: dan
--

CREATE TABLE public.system_branch (
    id integer NOT NULL,
    system_id integer NOT NULL,
    branch_name text
);


ALTER TABLE public.system_branch OWNER TO dan;

--
-- Name: system_branch_element_revision; Type: TABLE; Schema: public; Owner: dan
--

CREATE TABLE public.system_branch_element_revision (
    system_branch_id integer NOT NULL,
    element_id integer NOT NULL,
    revision_name text NOT NULL
);


ALTER TABLE public.system_branch_element_revision OWNER TO dan;

--
-- Name: system_branch_id_seq; Type: SEQUENCE; Schema: public; Owner: dan
--

CREATE SEQUENCE public.system_branch_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.system_branch_id_seq OWNER TO dan;

--
-- Name: system_branch_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: dan
--

ALTER SEQUENCE public.system_branch_id_seq OWNED BY public.system_branch.id;


--
-- Name: system_id_seq; Type: SEQUENCE; Schema: public; Owner: dan
--

CREATE SEQUENCE public.system_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.system_id_seq OWNER TO dan;

--
-- Name: system_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: dan
--

ALTER SEQUENCE public.system_id_seq OWNED BY public.system.id;


--
-- Name: tasks; Type: TABLE; Schema: public; Owner: dan
--

CREATE TABLE public.tasks (
    id integer NOT NULL,
    name text NOT NULL,
    description text NOT NULL
);


ALTER TABLE public.tasks OWNER TO dan;

--
-- Name: tasks_id_seq; Type: SEQUENCE; Schema: public; Owner: dan
--

CREATE SEQUENCE public.tasks_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.tasks_id_seq OWNER TO dan;

--
-- Name: tasks_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: dan
--

ALTER SEQUENCE public.tasks_id_seq OWNED BY public.tasks.id;


--
-- Name: user_confirmations; Type: TABLE; Schema: public; Owner: dan
--

CREATE TABLE public.user_confirmations (
    user_id integer NOT NULL,
    token text NOT NULL
);


ALTER TABLE public.user_confirmations OWNER TO dan;

--
-- Name: user_password_reset; Type: TABLE; Schema: public; Owner: dan
--

CREATE TABLE public.user_password_reset (
    user_id integer NOT NULL,
    date_requested timestamp with time zone DEFAULT ('now'::text)::timestamp(6) with time zone,
    ip_address inet NOT NULL,
    token text NOT NULL
);


ALTER TABLE public.user_password_reset OWNER TO dan;

--
-- Name: user_tasks; Type: TABLE; Schema: public; Owner: dan
--

CREATE TABLE public.user_tasks (
    user_id integer NOT NULL,
    task_id integer NOT NULL
);


ALTER TABLE public.user_tasks OWNER TO dan;

--
-- Name: users; Type: TABLE; Schema: public; Owner: dan
--

CREATE TABLE public.users (
    id integer NOT NULL,
    name text NOT NULL,
    cookie text NOT NULL,
    firstlogin timestamp with time zone DEFAULT ('now'::text)::timestamp(6) with time zone,
    lastlogin timestamp with time zone DEFAULT ('now'::text)::timestamp(6) with time zone,
    email text,
    watch_notice_id integer NOT NULL,
    emailsitenotices_yn boolean,
    emailbouncecount smallint DEFAULT 0,
    type character(1) DEFAULT 'U'::bpchar NOT NULL,
    status character(1) DEFAULT 'U'::bpchar NOT NULL,
    ip_address text NOT NULL,
    number_of_commits smallint,
    number_of_days smallint,
    watch_list_add_remove text DEFAULT 'default'::text,
    max_number_watch_lists integer DEFAULT 5,
    last_watch_list_chosen integer,
    page_size smallint DEFAULT 25,
    password_hash text NOT NULL,
    CONSTRAINT u_max_number_watch_lists CHECK (((max_number_watch_lists IS NOT NULL) AND (max_number_watch_lists >= 1))),
    CONSTRAINT u_watch_list_add_remove CHECK (((watch_list_add_remove = 'ask'::text) OR (watch_list_add_remove = 'default'::text))),
    CONSTRAINT users_status CHECK (((status = 'U'::bpchar) OR (status = 'A'::bpchar) OR (status = 'D'::bpchar))),
    CONSTRAINT users_type CHECK (((type = 'U'::bpchar) OR (type = 'S'::bpchar)))
);


ALTER TABLE public.users OWNER TO dan;

--
-- Name: users_id_seq; Type: SEQUENCE; Schema: public; Owner: dan
--

CREATE SEQUENCE public.users_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.users_id_seq OWNER TO dan;

--
-- Name: users_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: dan
--

ALTER SEQUENCE public.users_id_seq OWNED BY public.users.id;


--
-- Name: vuxml; Type: TABLE; Schema: public; Owner: dan
--

CREATE TABLE public.vuxml (
    id integer NOT NULL,
    vid text NOT NULL,
    topic text NOT NULL,
    description text NOT NULL,
    date_discovery text,
    date_entry text,
    date_modified text,
    status character(1) NOT NULL,
    checksum text
);


ALTER TABLE public.vuxml OWNER TO dan;

--
-- Name: vuxml_affected; Type: TABLE; Schema: public; Owner: dan
--

CREATE TABLE public.vuxml_affected (
    id integer NOT NULL,
    vuxml_id integer NOT NULL,
    type text NOT NULL
);


ALTER TABLE public.vuxml_affected OWNER TO dan;

--
-- Name: vuxml_affected_id_seq; Type: SEQUENCE; Schema: public; Owner: dan
--

CREATE SEQUENCE public.vuxml_affected_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.vuxml_affected_id_seq OWNER TO dan;

--
-- Name: vuxml_affected_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: dan
--

ALTER SEQUENCE public.vuxml_affected_id_seq OWNED BY public.vuxml_affected.id;


--
-- Name: vuxml_id_seq; Type: SEQUENCE; Schema: public; Owner: dan
--

CREATE SEQUENCE public.vuxml_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.vuxml_id_seq OWNER TO dan;

--
-- Name: vuxml_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: dan
--

ALTER SEQUENCE public.vuxml_id_seq OWNED BY public.vuxml.id;


--
-- Name: vuxml_names; Type: TABLE; Schema: public; Owner: dan
--

CREATE TABLE public.vuxml_names (
    id integer NOT NULL,
    vuxml_affected_id integer NOT NULL,
    name text NOT NULL
);


ALTER TABLE public.vuxml_names OWNER TO dan;

--
-- Name: vuxml_names_id_seq; Type: SEQUENCE; Schema: public; Owner: dan
--

CREATE SEQUENCE public.vuxml_names_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.vuxml_names_id_seq OWNER TO dan;

--
-- Name: vuxml_names_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: dan
--

ALTER SEQUENCE public.vuxml_names_id_seq OWNED BY public.vuxml_names.id;


--
-- Name: vuxml_ranges; Type: TABLE; Schema: public; Owner: dan
--

CREATE TABLE public.vuxml_ranges (
    id integer NOT NULL,
    vuxml_affected_id integer NOT NULL,
    operator1 text NOT NULL,
    version1 text NOT NULL,
    operator2 text,
    version2 text
);


ALTER TABLE public.vuxml_ranges OWNER TO dan;

--
-- Name: vuxml_ranges_id_seq; Type: SEQUENCE; Schema: public; Owner: dan
--

CREATE SEQUENCE public.vuxml_ranges_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.vuxml_ranges_id_seq OWNER TO dan;

--
-- Name: vuxml_ranges_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: dan
--

ALTER SEQUENCE public.vuxml_ranges_id_seq OWNED BY public.vuxml_ranges.id;


--
-- Name: vuxml_references; Type: TABLE; Schema: public; Owner: dan
--

CREATE TABLE public.vuxml_references (
    id integer NOT NULL,
    vuxml_id integer NOT NULL,
    type text NOT NULL,
    reference text NOT NULL
);


ALTER TABLE public.vuxml_references OWNER TO dan;

--
-- Name: vuxml_references_id_seq; Type: SEQUENCE; Schema: public; Owner: dan
--

CREATE SEQUENCE public.vuxml_references_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.vuxml_references_id_seq OWNER TO dan;

--
-- Name: vuxml_references_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: dan
--

ALTER SEQUENCE public.vuxml_references_id_seq OWNED BY public.vuxml_references.id;


--
-- Name: watch_list; Type: TABLE; Schema: public; Owner: dan
--

CREATE TABLE public.watch_list (
    id integer NOT NULL,
    user_id integer NOT NULL,
    name text NOT NULL,
    awaiting_staging boolean DEFAULT false NOT NULL,
    in_service boolean DEFAULT false,
    token text DEFAULT public.generate_watch_list_token(),
    CONSTRAINT watch_list_awaiting_staging CHECK (((awaiting_staging = true) OR (awaiting_staging = false))),
    CONSTRAINT wl_in_service_not_null CHECK ((in_service IS NOT NULL))
);


ALTER TABLE public.watch_list OWNER TO dan;

--
-- Name: watch_list_element; Type: TABLE; Schema: public; Owner: dan
--

CREATE TABLE public.watch_list_element (
    watch_list_id integer NOT NULL,
    element_id integer NOT NULL
);
ALTER TABLE ONLY public.watch_list_element ALTER COLUMN watch_list_id SET STATISTICS 1000;
ALTER TABLE ONLY public.watch_list_element ALTER COLUMN element_id SET STATISTICS 1000;


ALTER TABLE public.watch_list_element OWNER TO dan;

--
-- Name: watch_list_id_seq; Type: SEQUENCE; Schema: public; Owner: dan
--

CREATE SEQUENCE public.watch_list_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.watch_list_id_seq OWNER TO dan;

--
-- Name: watch_list_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: dan
--

ALTER SEQUENCE public.watch_list_id_seq OWNED BY public.watch_list.id;


--
-- Name: watch_list_staging; Type: TABLE; Schema: public; Owner: dan
--

CREATE TABLE public.watch_list_staging (
    id integer NOT NULL,
    user_id integer NOT NULL,
    category text NOT NULL,
    port text NOT NULL,
    item_count integer NOT NULL,
    from_pkg_info boolean NOT NULL,
    from_watch_list boolean NOT NULL,
    element_id integer,
    CONSTRAINT watch_list_stag_from_watch_list CHECK (((from_watch_list = true) OR (from_watch_list = false))),
    CONSTRAINT watch_list_stagin_from_pkg_info CHECK (((from_pkg_info = true) OR (from_pkg_info = false)))
);


ALTER TABLE public.watch_list_staging OWNER TO dan;

--
-- Name: watch_list_staging_id_seq; Type: SEQUENCE; Schema: public; Owner: dan
--

CREATE SEQUENCE public.watch_list_staging_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.watch_list_staging_id_seq OWNER TO dan;

--
-- Name: watch_list_staging_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: dan
--

ALTER SEQUENCE public.watch_list_staging_id_seq OWNED BY public.watch_list_staging.id;


--
-- Name: watch_list_staging_log; Type: TABLE; Schema: public; Owner: dan
--

CREATE TABLE public.watch_list_staging_log (
    id integer NOT NULL,
    date_added timestamp with time zone DEFAULT ('now'::text)::timestamp(6) with time zone NOT NULL,
    user_id integer NOT NULL,
    action character(1) NOT NULL,
    count_total integer DEFAULT 0 NOT NULL,
    count_matches integer DEFAULT 0 NOT NULL,
    count_missing integer DEFAULT 0 NOT NULL,
    count_duplicates integer DEFAULT 0 NOT NULL,
    count_categories integer DEFAULT 0 NOT NULL,
    CONSTRAINT watch_list_staging_log_action CHECK (((action = 'U'::bpchar) OR (action = 'W'::bpchar) OR (action = 'C'::bpchar) OR (action = 'D'::bpchar)))
);


ALTER TABLE public.watch_list_staging_log OWNER TO dan;

--
-- Name: watch_list_staging_log_id_seq; Type: SEQUENCE; Schema: public; Owner: dan
--

CREATE SEQUENCE public.watch_list_staging_log_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.watch_list_staging_log_id_seq OWNER TO dan;

--
-- Name: watch_list_staging_log_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: dan
--

ALTER SEQUENCE public.watch_list_staging_log_id_seq OWNED BY public.watch_list_staging_log.id;


--
-- Name: watch_notice; Type: TABLE; Schema: public; Owner: dan
--

CREATE TABLE public.watch_notice (
    id integer NOT NULL,
    frequency character(1) NOT NULL,
    description text NOT NULL,
    last_sent timestamp with time zone,
    CONSTRAINT watch_notice_frequency CHECK (((frequency = 'Z'::bpchar) OR (frequency = 'D'::bpchar) OR (frequency = 'W'::bpchar) OR (frequency = 'F'::bpchar) OR (frequency = 'M'::bpchar)))
);


ALTER TABLE public.watch_notice OWNER TO dan;

--
-- Name: watch_notice_id_seq; Type: SEQUENCE; Schema: public; Owner: dan
--

CREATE SEQUENCE public.watch_notice_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.watch_notice_id_seq OWNER TO dan;

--
-- Name: watch_notice_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: dan
--

ALTER SEQUENCE public.watch_notice_id_seq OWNED BY public.watch_notice.id;


--
-- Name: watch_notice_log; Type: TABLE; Schema: public; Owner: dan
--

CREATE TABLE public.watch_notice_log (
    id integer NOT NULL,
    notice_date timestamp with time zone DEFAULT ('now'::text)::timestamp(6) with time zone NOT NULL,
    frequency_id integer NOT NULL,
    msg_count integer NOT NULL,
    commit_count integer NOT NULL
);


ALTER TABLE public.watch_notice_log OWNER TO dan;

--
-- Name: watch_notice_log_id_seq; Type: SEQUENCE; Schema: public; Owner: dan
--

CREATE SEQUENCE public.watch_notice_log_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.watch_notice_log_id_seq OWNER TO dan;

--
-- Name: watch_notice_log_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: dan
--

ALTER SEQUENCE public.watch_notice_log_id_seq OWNED BY public.watch_notice_log.id;


--
-- Name: announcements id; Type: DEFAULT; Schema: public; Owner: dan
--

ALTER TABLE ONLY public.announcements ALTER COLUMN id SET DEFAULT nextval('public.announcements_id_seq'::regclass);


--
-- Name: cache_clearing_dates id; Type: DEFAULT; Schema: public; Owner: dan
--

ALTER TABLE ONLY public.cache_clearing_dates ALTER COLUMN id SET DEFAULT nextval('public.cache_clearing_dates_id_seq'::regclass);


--
-- Name: cache_clearing_ports id; Type: DEFAULT; Schema: public; Owner: dan
--

ALTER TABLE ONLY public.cache_clearing_ports ALTER COLUMN id SET DEFAULT nextval('public.cache_clearing_ports_id_seq'::regclass);


--
-- Name: categories id; Type: DEFAULT; Schema: public; Owner: dan
--

ALTER TABLE ONLY public.categories ALTER COLUMN id SET DEFAULT nextval('public.categories_id_seq'::regclass);


--
-- Name: commit_log id; Type: DEFAULT; Schema: public; Owner: dan
--

ALTER TABLE ONLY public.commit_log ALTER COLUMN id SET DEFAULT nextval('public.commit_log_id_seq'::regclass);


--
-- Name: commit_log_elements id; Type: DEFAULT; Schema: public; Owner: dan
--

ALTER TABLE ONLY public.commit_log_elements ALTER COLUMN id SET DEFAULT nextval('public.commit_log_elements_id_seq'::regclass);


--
-- Name: commit_log_ports_vuxml id; Type: DEFAULT; Schema: public; Owner: dan
--

ALTER TABLE ONLY public.commit_log_ports_vuxml ALTER COLUMN id SET DEFAULT nextval('public.commit_log_ports_vuxml_id_seq'::regclass);


--
-- Name: design_results id; Type: DEFAULT; Schema: public; Owner: dan
--

ALTER TABLE ONLY public.design_results ALTER COLUMN id SET DEFAULT nextval('public.design_results_id_seq'::regclass);


--
-- Name: element id; Type: DEFAULT; Schema: public; Owner: dan
--

ALTER TABLE ONLY public.element ALTER COLUMN id SET DEFAULT nextval('public.element_id_seq'::regclass);


--
-- Name: graphs id; Type: DEFAULT; Schema: public; Owner: dan
--

ALTER TABLE ONLY public.graphs ALTER COLUMN id SET DEFAULT nextval('public.graphs_id_seq'::regclass);


--
-- Name: listen_for id; Type: DEFAULT; Schema: public; Owner: dan
--

ALTER TABLE ONLY public.listen_for ALTER COLUMN id SET DEFAULT nextval('public.listen_for_id_seq'::regclass);


--
-- Name: page_load_detail id; Type: DEFAULT; Schema: public; Owner: dan
--

ALTER TABLE ONLY public.page_load_detail ALTER COLUMN id SET DEFAULT nextval('public.page_load_detail_id_seq'::regclass);


--
-- Name: page_load_summary id; Type: DEFAULT; Schema: public; Owner: dan
--

ALTER TABLE ONLY public.page_load_summary ALTER COLUMN id SET DEFAULT nextval('public.page_load_summary_id_seq'::regclass);


--
-- Name: ports id; Type: DEFAULT; Schema: public; Owner: dan
--

ALTER TABLE ONLY public.ports ALTER COLUMN id SET DEFAULT nextval('public.ports_id_seq'::regclass);


--
-- Name: ports_check id; Type: DEFAULT; Schema: public; Owner: dan
--

ALTER TABLE ONLY public.ports_check ALTER COLUMN id SET DEFAULT nextval('public.ports_check_id_seq'::regclass);


--
-- Name: ports_conflicts id; Type: DEFAULT; Schema: public; Owner: dan
--

ALTER TABLE ONLY public.ports_conflicts ALTER COLUMN id SET DEFAULT nextval('public.ports_conflicts_id_seq'::regclass);


--
-- Name: ports_moved id; Type: DEFAULT; Schema: public; Owner: dan
--

ALTER TABLE ONLY public.ports_moved ALTER COLUMN id SET DEFAULT nextval('public.ports_moved_id_seq'::regclass);


--
-- Name: ports_updating id; Type: DEFAULT; Schema: public; Owner: dan
--

ALTER TABLE ONLY public.ports_updating ALTER COLUMN id SET DEFAULT nextval('public.ports_updating_id_seq'::regclass);


--
-- Name: repo id; Type: DEFAULT; Schema: public; Owner: dan
--

ALTER TABLE ONLY public.repo ALTER COLUMN id SET DEFAULT nextval('public.repo_id_seq'::regclass);


--
-- Name: report_frequency id; Type: DEFAULT; Schema: public; Owner: dan
--

ALTER TABLE ONLY public.report_frequency ALTER COLUMN id SET DEFAULT nextval('public.report_frequency_id_seq'::regclass);


--
-- Name: report_log id; Type: DEFAULT; Schema: public; Owner: dan
--

ALTER TABLE ONLY public.report_log ALTER COLUMN id SET DEFAULT nextval('public.report_log_id_seq'::regclass);


--
-- Name: reports id; Type: DEFAULT; Schema: public; Owner: dan
--

ALTER TABLE ONLY public.reports ALTER COLUMN id SET DEFAULT nextval('public.reports_id_seq'::regclass);


--
-- Name: sanity_test_failures id; Type: DEFAULT; Schema: public; Owner: dan
--

ALTER TABLE ONLY public.sanity_test_failures ALTER COLUMN id SET DEFAULT nextval('public.sanity_test_failures_id_seq'::regclass);


--
-- Name: security_notice id; Type: DEFAULT; Schema: public; Owner: dan
--

ALTER TABLE ONLY public.security_notice ALTER COLUMN id SET DEFAULT nextval('public.security_notice_id_seq'::regclass);


--
-- Name: security_notice_audit id; Type: DEFAULT; Schema: public; Owner: dan
--

ALTER TABLE ONLY public.security_notice_audit ALTER COLUMN id SET DEFAULT nextval('public.security_notice_audit_id_seq'::regclass);


--
-- Name: system id; Type: DEFAULT; Schema: public; Owner: dan
--

ALTER TABLE ONLY public.system ALTER COLUMN id SET DEFAULT nextval('public.system_id_seq'::regclass);


--
-- Name: system_branch id; Type: DEFAULT; Schema: public; Owner: dan
--

ALTER TABLE ONLY public.system_branch ALTER COLUMN id SET DEFAULT nextval('public.system_branch_id_seq'::regclass);


--
-- Name: tasks id; Type: DEFAULT; Schema: public; Owner: dan
--

ALTER TABLE ONLY public.tasks ALTER COLUMN id SET DEFAULT nextval('public.tasks_id_seq'::regclass);


--
-- Name: users id; Type: DEFAULT; Schema: public; Owner: dan
--

ALTER TABLE ONLY public.users ALTER COLUMN id SET DEFAULT nextval('public.users_id_seq'::regclass);


--
-- Name: vuxml id; Type: DEFAULT; Schema: public; Owner: dan
--

ALTER TABLE ONLY public.vuxml ALTER COLUMN id SET DEFAULT nextval('public.vuxml_id_seq'::regclass);


--
-- Name: vuxml_affected id; Type: DEFAULT; Schema: public; Owner: dan
--

ALTER TABLE ONLY public.vuxml_affected ALTER COLUMN id SET DEFAULT nextval('public.vuxml_affected_id_seq'::regclass);


--
-- Name: vuxml_names id; Type: DEFAULT; Schema: public; Owner: dan
--

ALTER TABLE ONLY public.vuxml_names ALTER COLUMN id SET DEFAULT nextval('public.vuxml_names_id_seq'::regclass);


--
-- Name: vuxml_ranges id; Type: DEFAULT; Schema: public; Owner: dan
--

ALTER TABLE ONLY public.vuxml_ranges ALTER COLUMN id SET DEFAULT nextval('public.vuxml_ranges_id_seq'::regclass);


--
-- Name: vuxml_references id; Type: DEFAULT; Schema: public; Owner: dan
--

ALTER TABLE ONLY public.vuxml_references ALTER COLUMN id SET DEFAULT nextval('public.vuxml_references_id_seq'::regclass);


--
-- Name: watch_list id; Type: DEFAULT; Schema: public; Owner: dan
--

ALTER TABLE ONLY public.watch_list ALTER COLUMN id SET DEFAULT nextval('public.watch_list_id_seq'::regclass);


--
-- Name: watch_list_staging id; Type: DEFAULT; Schema: public; Owner: dan
--

ALTER TABLE ONLY public.watch_list_staging ALTER COLUMN id SET DEFAULT nextval('public.watch_list_staging_id_seq'::regclass);


--
-- Name: watch_list_staging_log id; Type: DEFAULT; Schema: public; Owner: dan
--

ALTER TABLE ONLY public.watch_list_staging_log ALTER COLUMN id SET DEFAULT nextval('public.watch_list_staging_log_id_seq'::regclass);


--
-- Name: watch_notice id; Type: DEFAULT; Schema: public; Owner: dan
--

ALTER TABLE ONLY public.watch_notice ALTER COLUMN id SET DEFAULT nextval('public.watch_notice_id_seq'::regclass);


--
-- Name: watch_notice_log id; Type: DEFAULT; Schema: public; Owner: dan
--

ALTER TABLE ONLY public.watch_notice_log ALTER COLUMN id SET DEFAULT nextval('public.watch_notice_log_id_seq'::regclass);


--
-- Name: announcements announcements_pkey; Type: CONSTRAINT; Schema: public; Owner: dan
--

ALTER TABLE ONLY public.announcements
    ADD CONSTRAINT announcements_pkey PRIMARY KEY (id);


--
-- Name: cache_clearing_ports cache_clearing_ports_pkey; Type: CONSTRAINT; Schema: public; Owner: dan
--

ALTER TABLE ONLY public.cache_clearing_ports
    ADD CONSTRAINT cache_clearing_ports_pkey PRIMARY KEY (id);


--
-- Name: categories categories_pkey; Type: CONSTRAINT; Schema: public; Owner: dan
--

ALTER TABLE ONLY public.categories
    ADD CONSTRAINT categories_pkey PRIMARY KEY (id);


--
-- Name: category_stats category_stats_pkey; Type: CONSTRAINT; Schema: public; Owner: dan
--

ALTER TABLE ONLY public.category_stats
    ADD CONSTRAINT category_stats_pkey PRIMARY KEY (category_id);


--
-- Name: commit_log_branch_delete_me commit_log_branch_pkey; Type: CONSTRAINT; Schema: public; Owner: dan
--

ALTER TABLE ONLY public.commit_log_branch_delete_me
    ADD CONSTRAINT commit_log_branch_pkey PRIMARY KEY (commit_log_id, branch_id);


--
-- Name: commit_log_elements commit_log_elements_pkey; Type: CONSTRAINT; Schema: public; Owner: dan
--

ALTER TABLE ONLY public.commit_log_elements
    ADD CONSTRAINT commit_log_elements_pkey PRIMARY KEY (id);


--
-- Name: commit_log commit_log_message_id; Type: CONSTRAINT; Schema: public; Owner: dan
--

ALTER TABLE ONLY public.commit_log
    ADD CONSTRAINT commit_log_message_id UNIQUE (message_id);


--
-- Name: commit_log commit_log_pkey; Type: CONSTRAINT; Schema: public; Owner: dan
--

ALTER TABLE ONLY public.commit_log
    ADD CONSTRAINT commit_log_pkey PRIMARY KEY (id);


--
-- Name: commit_log_port_elements commit_log_port_elements_pkey; Type: CONSTRAINT; Schema: public; Owner: dan
--

ALTER TABLE ONLY public.commit_log_port_elements
    ADD CONSTRAINT commit_log_port_elements_pkey PRIMARY KEY (commit_log_id, port_id, commit_log_element_id);


--
-- Name: commit_log_ports_elements commit_log_ports_elements_pkey; Type: CONSTRAINT; Schema: public; Owner: dan
--

ALTER TABLE ONLY public.commit_log_ports_elements
    ADD CONSTRAINT commit_log_ports_elements_pkey PRIMARY KEY (commit_log_id, element_id);


--
-- Name: commit_log_ports_ignore commit_log_ports_ignore_pkey; Type: CONSTRAINT; Schema: public; Owner: dan
--

ALTER TABLE ONLY public.commit_log_ports_ignore
    ADD CONSTRAINT commit_log_ports_ignore_pkey PRIMARY KEY (commit_log_id, port_id);


--
-- Name: commit_log_ports commit_log_ports_pkey; Type: CONSTRAINT; Schema: public; Owner: dan
--

ALTER TABLE ONLY public.commit_log_ports
    ADD CONSTRAINT commit_log_ports_pkey PRIMARY KEY (commit_log_id, port_id);


--
-- Name: commit_log_ports_vuxml commit_log_ports_vuxml_pkey; Type: CONSTRAINT; Schema: public; Owner: dan
--

ALTER TABLE ONLY public.commit_log_ports_vuxml
    ADD CONSTRAINT commit_log_ports_vuxml_pkey PRIMARY KEY (id);


--
-- Name: commits_flagged commits_flagged_pkey; Type: CONSTRAINT; Schema: public; Owner: dan
--

ALTER TABLE ONLY public.commits_flagged
    ADD CONSTRAINT commits_flagged_pkey PRIMARY KEY (user_id, commit_log_id);


--
-- Name: commits_latest_ports commits_latest_ports_pkey; Type: CONSTRAINT; Schema: public; Owner: dan
--

ALTER TABLE ONLY public.commits_latest_ports
    ADD CONSTRAINT commits_latest_ports_pkey PRIMARY KEY (commit_log_id);


--
-- Name: committer_notify committer_notify_pkey; Type: CONSTRAINT; Schema: public; Owner: dan
--

ALTER TABLE ONLY public.committer_notify
    ADD CONSTRAINT committer_notify_pkey PRIMARY KEY (user_id);


--
-- Name: daily_refreshes daily_refreshes_pkey; Type: CONSTRAINT; Schema: public; Owner: dan
--

ALTER TABLE ONLY public.daily_refreshes
    ADD CONSTRAINT daily_refreshes_pkey PRIMARY KEY (refresh_date);


--
-- Name: daily_stats_data daily_stats_data_pkey; Type: CONSTRAINT; Schema: public; Owner: dan
--

ALTER TABLE ONLY public.daily_stats_data
    ADD CONSTRAINT daily_stats_data_pkey PRIMARY KEY (id);


--
-- Name: daily_stats_data daily_stats_data_unique; Type: CONSTRAINT; Schema: public; Owner: dan
--

ALTER TABLE ONLY public.daily_stats_data
    ADD CONSTRAINT daily_stats_data_unique UNIQUE (daily_stats_id, date);


--
-- Name: daily_stats daily_stats_pkey; Type: CONSTRAINT; Schema: public; Owner: dan
--

ALTER TABLE ONLY public.daily_stats
    ADD CONSTRAINT daily_stats_pkey PRIMARY KEY (id);


--
-- Name: element element_pkey; Type: CONSTRAINT; Schema: public; Owner: dan
--

ALTER TABLE ONLY public.element
    ADD CONSTRAINT element_pkey PRIMARY KEY (id);


--
-- Name: element_revision element_revision_pkey; Type: CONSTRAINT; Schema: public; Owner: dan
--

ALTER TABLE ONLY public.element_revision
    ADD CONSTRAINT element_revision_pkey PRIMARY KEY (element_id, revision_name);


--
-- Name: flavors flavors_name_unique; Type: CONSTRAINT; Schema: public; Owner: dan
--

ALTER TABLE ONLY public.flavors
    ADD CONSTRAINT flavors_name_unique UNIQUE (name);


--
-- Name: flavors flavors_pkey; Type: CONSTRAINT; Schema: public; Owner: dan
--

ALTER TABLE ONLY public.flavors
    ADD CONSTRAINT flavors_pkey PRIMARY KEY (id);


--
-- Name: graphs graphs_pkey; Type: CONSTRAINT; Schema: public; Owner: dan
--

ALTER TABLE ONLY public.graphs
    ADD CONSTRAINT graphs_pkey PRIMARY KEY (id);


--
-- Name: latest_commits latest_commits_pkey; Type: CONSTRAINT; Schema: public; Owner: dan
--

ALTER TABLE ONLY public.latest_commits
    ADD CONSTRAINT latest_commits_pkey PRIMARY KEY (commit_log_id);


--
-- Name: latest_commits_ports latest_commits_ports_pkey; Type: CONSTRAINT; Schema: public; Owner: dan
--

ALTER TABLE ONLY public.latest_commits_ports
    ADD CONSTRAINT latest_commits_ports_pkey PRIMARY KEY (commit_log_id);


--
-- Name: listen_for listen_for_pkey; Type: CONSTRAINT; Schema: public; Owner: dan
--

ALTER TABLE ONLY public.listen_for
    ADD CONSTRAINT listen_for_pkey PRIMARY KEY (id);


--
-- Name: page_load_detail page_load_detail_test_pkey; Type: CONSTRAINT; Schema: public; Owner: dan
--

ALTER TABLE ONLY public.page_load_detail
    ADD CONSTRAINT page_load_detail_test_pkey PRIMARY KEY (id);


--
-- Name: page_load_summary page_load_summary_pkey; Type: CONSTRAINT; Schema: public; Owner: dan
--

ALTER TABLE ONLY public.page_load_summary
    ADD CONSTRAINT page_load_summary_pkey PRIMARY KEY (id);


--
-- Name: port_dependencies port_dependencies_pkey; Type: CONSTRAINT; Schema: public; Owner: dan
--

ALTER TABLE ONLY public.port_dependencies
    ADD CONSTRAINT port_dependencies_pkey PRIMARY KEY (port_id, port_id_dependent_upon, dependency_type);


--
-- Name: ports_categories ports_categories_pkey; Type: CONSTRAINT; Schema: public; Owner: dan
--

ALTER TABLE ONLY public.ports_categories
    ADD CONSTRAINT ports_categories_pkey PRIMARY KEY (port_id, category_id);


--
-- Name: ports_check ports_check_pkey; Type: CONSTRAINT; Schema: public; Owner: dan
--

ALTER TABLE ONLY public.ports_check
    ADD CONSTRAINT ports_check_pkey PRIMARY KEY (id);


--
-- Name: ports_conflicts_matches ports_conflicts_matches_pkey; Type: CONSTRAINT; Schema: public; Owner: dan
--

ALTER TABLE ONLY public.ports_conflicts_matches
    ADD CONSTRAINT ports_conflicts_matches_pkey PRIMARY KEY (ports_conflicts_id, port_id);


--
-- Name: ports_conflicts ports_conflicts_pkey; Type: CONSTRAINT; Schema: public; Owner: dan
--

ALTER TABLE ONLY public.ports_conflicts
    ADD CONSTRAINT ports_conflicts_pkey PRIMARY KEY (id);


--
-- Name: ports_moved ports_moved_pkey; Type: CONSTRAINT; Schema: public; Owner: dan
--

ALTER TABLE ONLY public.ports_moved
    ADD CONSTRAINT ports_moved_pkey PRIMARY KEY (id);


--
-- Name: ports ports_pkey; Type: CONSTRAINT; Schema: public; Owner: dan
--

ALTER TABLE ONLY public.ports
    ADD CONSTRAINT ports_pkey PRIMARY KEY (id);


--
-- Name: ports_updating ports_updating_pkey; Type: CONSTRAINT; Schema: public; Owner: dan
--

ALTER TABLE ONLY public.ports_updating
    ADD CONSTRAINT ports_updating_pkey PRIMARY KEY (id);


--
-- Name: ports_vulnerable ports_vulnerable_pkey; Type: CONSTRAINT; Schema: public; Owner: dan
--

ALTER TABLE ONLY public.ports_vulnerable
    ADD CONSTRAINT ports_vulnerable_pkey PRIMARY KEY (port_id);


--
-- Name: report_frequency report_frequency_pkey; Type: CONSTRAINT; Schema: public; Owner: dan
--

ALTER TABLE ONLY public.report_frequency
    ADD CONSTRAINT report_frequency_pkey PRIMARY KEY (id);


--
-- Name: report_log report_log_pkey; Type: CONSTRAINT; Schema: public; Owner: dan
--

ALTER TABLE ONLY public.report_log
    ADD CONSTRAINT report_log_pkey PRIMARY KEY (id);


--
-- Name: report_subscriptions report_subscriptions_pkey; Type: CONSTRAINT; Schema: public; Owner: dan
--

ALTER TABLE ONLY public.report_subscriptions
    ADD CONSTRAINT report_subscriptions_pkey PRIMARY KEY (report_id, user_id);


--
-- Name: reports reports_pkey; Type: CONSTRAINT; Schema: public; Owner: dan
--

ALTER TABLE ONLY public.reports
    ADD CONSTRAINT reports_pkey PRIMARY KEY (id);


--
-- Name: sanity_test_failures sanity_test_failures_pkey; Type: CONSTRAINT; Schema: public; Owner: dan
--

ALTER TABLE ONLY public.sanity_test_failures
    ADD CONSTRAINT sanity_test_failures_pkey PRIMARY KEY (id);


--
-- Name: security_notice_audit security_notice_audit_pkey; Type: CONSTRAINT; Schema: public; Owner: dan
--

ALTER TABLE ONLY public.security_notice_audit
    ADD CONSTRAINT security_notice_audit_pkey PRIMARY KEY (id);


--
-- Name: security_notice security_notice_pkey; Type: CONSTRAINT; Schema: public; Owner: dan
--

ALTER TABLE ONLY public.security_notice
    ADD CONSTRAINT security_notice_pkey PRIMARY KEY (id);


--
-- Name: security_notice_status security_notice_status_pkey; Type: CONSTRAINT; Schema: public; Owner: dan
--

ALTER TABLE ONLY public.security_notice_status
    ADD CONSTRAINT security_notice_status_pkey PRIMARY KEY (id);


--
-- Name: system_branch_element_revision system_branch_element_revi_pkey; Type: CONSTRAINT; Schema: public; Owner: dan
--

ALTER TABLE ONLY public.system_branch_element_revision
    ADD CONSTRAINT system_branch_element_revi_pkey PRIMARY KEY (system_branch_id, element_id, revision_name);


--
-- Name: system_branch system_branch_pkey; Type: CONSTRAINT; Schema: public; Owner: dan
--

ALTER TABLE ONLY public.system_branch
    ADD CONSTRAINT system_branch_pkey PRIMARY KEY (id);


--
-- Name: system system_pkey; Type: CONSTRAINT; Schema: public; Owner: dan
--

ALTER TABLE ONLY public.system
    ADD CONSTRAINT system_pkey PRIMARY KEY (id);


--
-- Name: tasks tasks_pkey; Type: CONSTRAINT; Schema: public; Owner: dan
--

ALTER TABLE ONLY public.tasks
    ADD CONSTRAINT tasks_pkey PRIMARY KEY (id);


--
-- Name: user_confirmations user_confirmations_pkey; Type: CONSTRAINT; Schema: public; Owner: dan
--

ALTER TABLE ONLY public.user_confirmations
    ADD CONSTRAINT user_confirmations_pkey PRIMARY KEY (user_id, token);


--
-- Name: design_results user_id; Type: CONSTRAINT; Schema: public; Owner: dan
--

ALTER TABLE ONLY public.design_results
    ADD CONSTRAINT user_id UNIQUE (user_id);


--
-- Name: user_tasks user_tasks_pkey; Type: CONSTRAINT; Schema: public; Owner: dan
--

ALTER TABLE ONLY public.user_tasks
    ADD CONSTRAINT user_tasks_pkey PRIMARY KEY (task_id, user_id);


--
-- Name: users users_name; Type: CONSTRAINT; Schema: public; Owner: dan
--

ALTER TABLE ONLY public.users
    ADD CONSTRAINT users_name UNIQUE (name);


--
-- Name: users users_pkey; Type: CONSTRAINT; Schema: public; Owner: dan
--

ALTER TABLE ONLY public.users
    ADD CONSTRAINT users_pkey PRIMARY KEY (id);


--
-- Name: vuxml_affected vuxml_affected_pkey; Type: CONSTRAINT; Schema: public; Owner: dan
--

ALTER TABLE ONLY public.vuxml_affected
    ADD CONSTRAINT vuxml_affected_pkey PRIMARY KEY (id);


--
-- Name: vuxml_names vuxml_names_pkey; Type: CONSTRAINT; Schema: public; Owner: dan
--

ALTER TABLE ONLY public.vuxml_names
    ADD CONSTRAINT vuxml_names_pkey PRIMARY KEY (id);


--
-- Name: vuxml vuxml_pkey; Type: CONSTRAINT; Schema: public; Owner: dan
--

ALTER TABLE ONLY public.vuxml
    ADD CONSTRAINT vuxml_pkey PRIMARY KEY (id);


--
-- Name: vuxml_ranges vuxml_ranges_pkey; Type: CONSTRAINT; Schema: public; Owner: dan
--

ALTER TABLE ONLY public.vuxml_ranges
    ADD CONSTRAINT vuxml_ranges_pkey PRIMARY KEY (id);


--
-- Name: vuxml_references vuxml_references_pkey; Type: CONSTRAINT; Schema: public; Owner: dan
--

ALTER TABLE ONLY public.vuxml_references
    ADD CONSTRAINT vuxml_references_pkey PRIMARY KEY (id);


--
-- Name: watch_list_element watch_list_element_pkey; Type: CONSTRAINT; Schema: public; Owner: dan
--

ALTER TABLE ONLY public.watch_list_element
    ADD CONSTRAINT watch_list_element_pkey PRIMARY KEY (watch_list_id, element_id);


--
-- Name: watch_list watch_list_pkey; Type: CONSTRAINT; Schema: public; Owner: dan
--

ALTER TABLE ONLY public.watch_list
    ADD CONSTRAINT watch_list_pkey PRIMARY KEY (id);


--
-- Name: watch_list_staging_log watch_list_staging_log_pkey; Type: CONSTRAINT; Schema: public; Owner: dan
--

ALTER TABLE ONLY public.watch_list_staging_log
    ADD CONSTRAINT watch_list_staging_log_pkey PRIMARY KEY (id);


--
-- Name: watch_list_staging watch_list_staging_pkey; Type: CONSTRAINT; Schema: public; Owner: dan
--

ALTER TABLE ONLY public.watch_list_staging
    ADD CONSTRAINT watch_list_staging_pkey PRIMARY KEY (id);


--
-- Name: watch_notice_log watch_notice_log_pkey; Type: CONSTRAINT; Schema: public; Owner: dan
--

ALTER TABLE ONLY public.watch_notice_log
    ADD CONSTRAINT watch_notice_log_pkey PRIMARY KEY (id);


--
-- Name: watch_notice watch_notice_pkey; Type: CONSTRAINT; Schema: public; Owner: dan
--

ALTER TABLE ONLY public.watch_notice
    ADD CONSTRAINT watch_notice_pkey PRIMARY KEY (id);


--
-- Name: commit_log_branches_pk; Type: INDEX; Schema: public; Owner: dan
--

CREATE UNIQUE INDEX commit_log_branches_pk ON public.commit_log_branches USING btree (commit_log_id, branch_id);


--
-- Name: commit_log_commit_date; Type: INDEX; Schema: public; Owner: dan
--

CREATE INDEX commit_log_commit_date ON public.commit_log USING btree (commit_date);


--
-- Name: commit_log_committer_idx; Type: INDEX; Schema: public; Owner: dan
--

CREATE INDEX commit_log_committer_idx ON public.commit_log USING btree (committer);


--
-- Name: commit_log_elements_clid; Type: INDEX; Schema: public; Owner: dan
--

CREATE INDEX commit_log_elements_clid ON public.commit_log_elements USING btree (commit_log_id);


--
-- Name: commit_log_elements_ei; Type: INDEX; Schema: public; Owner: dan
--

CREATE INDEX commit_log_elements_ei ON public.commit_log_elements USING btree (element_id);


--
-- Name: commit_log_ports_elements_clid; Type: INDEX; Schema: public; Owner: dan
--

CREATE INDEX commit_log_ports_elements_clid ON public.commit_log_ports_elements USING btree (commit_log_id);


--
-- Name: commit_log_ports_needs_refresh_idx; Type: INDEX; Schema: public; Owner: dan
--

CREATE INDEX commit_log_ports_needs_refresh_idx ON public.commit_log_ports USING btree (needs_refresh) WHERE (needs_refresh <> 0);


--
-- Name: commit_log_ports_port_id; Type: INDEX; Schema: public; Owner: dan
--

CREATE INDEX commit_log_ports_port_id ON public.commit_log_ports USING btree (port_id);


--
-- Name: commit_log_ports_port_name_revision; Type: INDEX; Schema: public; Owner: dan
--

CREATE INDEX commit_log_ports_port_name_revision ON public.commit_log_ports USING btree (port_name_revision);


--
-- Name: commit_log_ports_vuxml_port_id; Type: INDEX; Schema: public; Owner: dan
--

CREATE INDEX commit_log_ports_vuxml_port_id ON public.commit_log_ports_vuxml USING btree (port_id);


--
-- Name: commit_log_ports_vuxml_vuxml_id_idx; Type: INDEX; Schema: public; Owner: dan
--

CREATE INDEX commit_log_ports_vuxml_vuxml_id_idx ON public.commit_log_ports_vuxml USING btree (vuxml_id);


--
-- Name: commit_log_svn_revision_idx; Type: INDEX; Schema: public; Owner: dan
--

CREATE INDEX commit_log_svn_revision_idx ON public.commit_log USING btree (svn_revision) WHERE (svn_revision IS NOT NULL);


--
-- Name: element_delete; Type: INDEX; Schema: public; Owner: dan
--

CREATE INDEX element_delete ON public.element USING btree (status) WHERE (status = 'D'::bpchar);


--
-- Name: element_name; Type: INDEX; Schema: public; Owner: dan
--

CREATE INDEX element_name ON public.element USING btree (name);


--
-- Name: element_parent_id; Type: INDEX; Schema: public; Owner: dan
--

CREATE INDEX element_parent_id ON public.element USING btree (parent_id);


--
-- Name: element_pathname_element_id; Type: INDEX; Schema: public; Owner: dan
--

CREATE INDEX element_pathname_element_id ON public.element_pathname USING btree (element_id);


--
-- Name: element_pathname_pathname; Type: INDEX; Schema: public; Owner: dan
--

CREATE UNIQUE INDEX element_pathname_pathname ON public.element_pathname USING btree (pathname);


--
-- Name: fki_design_results_user_id; Type: INDEX; Schema: public; Owner: dan
--

CREATE INDEX fki_design_results_user_id ON public.design_results USING btree (user_id);


--
-- Name: fki_package_flavors_flavor_id_fk; Type: INDEX; Schema: public; Owner: dan
--

CREATE INDEX fki_package_flavors_flavor_id_fk ON public.package_flavors USING btree (flavor_id);


--
-- Name: fki_package_flavors_port_id_fk; Type: INDEX; Schema: public; Owner: dan
--

CREATE INDEX fki_package_flavors_port_id_fk ON public.package_flavors USING btree (port_id);


--
-- Name: fki_ports_conflicts_matches_conflicts_id; Type: INDEX; Schema: public; Owner: dan
--

CREATE INDEX fki_ports_conflicts_matches_conflicts_id ON public.ports_conflicts_matches USING btree (ports_conflicts_id);


--
-- Name: fki_ports_conflicts_matches_port_id; Type: INDEX; Schema: public; Owner: dan
--

CREATE INDEX fki_ports_conflicts_matches_port_id ON public.ports_conflicts_matches USING btree (port_id);


--
-- Name: fki_ports_conflicts_port_id; Type: INDEX; Schema: public; Owner: dan
--

CREATE INDEX fki_ports_conflicts_port_id ON public.ports_conflicts USING btree (port_id);


--
-- Name: flavors_name_idx; Type: INDEX; Schema: public; Owner: dan
--

CREATE UNIQUE INDEX flavors_name_idx ON public.flavors USING btree (name);


--
-- Name: generate_plist_installed_file_gin_idx; Type: INDEX; Schema: public; Owner: dan
--

CREATE INDEX generate_plist_installed_file_gin_idx ON public.generate_plist USING gin (to_tsvector('english'::regconfig, installed_file));


--
-- Name: generate_plist_installed_file_idx; Type: INDEX; Schema: public; Owner: dan
--

CREATE INDEX generate_plist_installed_file_idx ON public.generate_plist USING btree (installed_file);


--
-- Name: generate_plist_pk; Type: INDEX; Schema: public; Owner: dan
--

CREATE INDEX generate_plist_pk ON public.generate_plist USING btree (id);


--
-- Name: generate_plist_port_id_idx; Type: INDEX; Schema: public; Owner: dan
--

CREATE INDEX generate_plist_port_id_idx ON public.generate_plist USING btree (port_id);


--
-- Name: listen_for_name_idx; Type: INDEX; Schema: public; Owner: dan
--

CREATE UNIQUE INDEX listen_for_name_idx ON public.listen_for USING btree (name);


--
-- Name: needs_refresh; Type: INDEX; Schema: public; Owner: dan
--

CREATE INDEX needs_refresh ON public.commit_log_ports USING btree (needs_refresh);


--
-- Name: package_flavors_port_id_flavor_id_idx; Type: INDEX; Schema: public; Owner: dan
--

CREATE UNIQUE INDEX package_flavors_port_id_flavor_id_idx ON public.package_flavors USING btree (port_id, flavor_id);


--
-- Name: page_load_detail_date; Type: INDEX; Schema: public; Owner: dan
--

CREATE INDEX page_load_detail_date ON public.page_load_detail USING btree (date);


--
-- Name: page_load_ip_address; Type: INDEX; Schema: public; Owner: dan
--

CREATE INDEX page_load_ip_address ON public.page_load_detail USING btree (ip_address);


--
-- Name: page_loads_date_date; Type: INDEX; Schema: public; Owner: dan
--

CREATE UNIQUE INDEX page_loads_date_date ON public.page_load_summary USING btree (date, page_name);


--
-- Name: ports_active_idx; Type: INDEX; Schema: public; Owner: dan
--

CREATE INDEX ports_active_idx ON public.ports USING btree (status) WHERE (status = 'A'::bpchar);


--
-- Name: ports_broken; Type: INDEX; Schema: public; Owner: dan
--

CREATE INDEX ports_broken ON public.ports USING btree (broken) WHERE (broken <> ''::text);


--
-- Name: ports_categories_categories_idx; Type: INDEX; Schema: public; Owner: dan
--

CREATE INDEX ports_categories_categories_idx ON public.ports_categories USING btree (category_id);


--
-- Name: ports_categories_ports_idx; Type: INDEX; Schema: public; Owner: dan
--

CREATE INDEX ports_categories_ports_idx ON public.ports_categories USING btree (port_id);


--
-- Name: ports_category_id_idx; Type: INDEX; Schema: public; Owner: dan
--

CREATE INDEX ports_category_id_idx ON public.ports USING btree (category_id);


--
-- Name: ports_check_category_id; Type: INDEX; Schema: public; Owner: dan
--

CREATE INDEX ports_check_category_id ON public.ports_check USING btree (category_id);


--
-- Name: ports_date_added_idx; Type: INDEX; Schema: public; Owner: dan
--

CREATE INDEX ports_date_added_idx ON public.ports USING btree (date_added);


--
-- Name: ports_deleted; Type: INDEX; Schema: public; Owner: dan
--

CREATE INDEX ports_deleted ON public.ports USING btree (status) WHERE (status = 'D'::bpchar);


--
-- Name: ports_element_id; Type: INDEX; Schema: public; Owner: dan
--

CREATE INDEX ports_element_id ON public.ports USING btree (element_id);


--
-- Name: ports_expiration_date; Type: INDEX; Schema: public; Owner: dan
--

CREATE INDEX ports_expiration_date ON public.ports USING btree (expiration_date) WHERE (expiration_date IS NOT NULL);


--
-- Name: ports_ignore; Type: INDEX; Schema: public; Owner: dan
--

CREATE INDEX ports_ignore ON public.ports USING btree (ignore) WHERE (ignore <> ''::text);


--
-- Name: ports_is_interactive; Type: INDEX; Schema: public; Owner: dan
--

CREATE INDEX ports_is_interactive ON public.ports USING btree (is_interactive) WHERE (is_interactive IS NOT NULL);


--
-- Name: ports_maintainer_idx; Type: INDEX; Schema: public; Owner: dan
--

CREATE INDEX ports_maintainer_idx ON public.ports USING btree (lower(maintainer));


--
-- Name: ports_package_name; Type: INDEX; Schema: public; Owner: dan
--

CREATE INDEX ports_package_name ON public.ports USING btree (package_name);


--
-- Name: ports_ports_expiration_date; Type: INDEX; Schema: public; Owner: dan
--

CREATE INDEX ports_ports_expiration_date ON public.ports USING btree (expiration_date) WHERE (expiration_date IS NOT NULL);


--
-- Name: repo_description; Type: INDEX; Schema: public; Owner: dan
--

CREATE UNIQUE INDEX repo_description ON public.repo USING btree (description);


--
-- Name: repo_id; Type: INDEX; Schema: public; Owner: dan
--

CREATE UNIQUE INDEX repo_id ON public.repo USING btree (id);


--
-- Name: repo_name; Type: INDEX; Schema: public; Owner: dan
--

CREATE UNIQUE INDEX repo_name ON public.repo USING btree (name);


--
-- Name: repo_path_to_repo; Type: INDEX; Schema: public; Owner: dan
--

CREATE UNIQUE INDEX repo_path_to_repo ON public.repo USING btree (path_to_repo);


--
-- Name: security_notice_clid_idx; Type: INDEX; Schema: public; Owner: dan
--

CREATE UNIQUE INDEX security_notice_clid_idx ON public.security_notice USING btree (commit_log_id);


--
-- Name: system_branch_element_revision_element_id_idx; Type: INDEX; Schema: public; Owner: dan
--

CREATE INDEX system_branch_element_revision_element_id_idx ON public.system_branch_element_revision USING btree (element_id);


--
-- Name: tasks_idx; Type: INDEX; Schema: public; Owner: dan
--

CREATE UNIQUE INDEX tasks_idx ON public.tasks USING btree (name);


--
-- Name: users_cookie; Type: INDEX; Schema: public; Owner: dan
--

CREATE INDEX users_cookie ON public.users USING btree (cookie);


--
-- Name: users_email; Type: INDEX; Schema: public; Owner: dan
--

CREATE INDEX users_email ON public.users USING btree (email);


--
-- Name: vuxml_names_name; Type: INDEX; Schema: public; Owner: dan
--

CREATE INDEX vuxml_names_name ON public.vuxml_names USING btree (name);


--
-- Name: vuxml_vid_idx; Type: INDEX; Schema: public; Owner: dan
--

CREATE INDEX vuxml_vid_idx ON public.vuxml USING btree (vid);


--
-- Name: watch_list_element_element_id; Type: INDEX; Schema: public; Owner: dan
--

CREATE INDEX watch_list_element_element_id ON public.watch_list_element USING btree (element_id);


--
-- Name: watch_list_element_watch_list_id; Type: INDEX; Schema: public; Owner: dan
--

CREATE INDEX watch_list_element_watch_list_id ON public.watch_list_element USING btree (watch_list_id);


--
-- Name: watch_list_token; Type: INDEX; Schema: public; Owner: dan
--

CREATE UNIQUE INDEX watch_list_token ON public.watch_list USING btree (token);


--
-- Name: watch_list_user_id; Type: INDEX; Schema: public; Owner: dan
--

CREATE INDEX watch_list_user_id ON public.watch_list USING btree (user_id);


--
-- Name: watch_notice_frequency; Type: INDEX; Schema: public; Owner: dan
--

CREATE INDEX watch_notice_frequency ON public.watch_notice USING btree (frequency);


--
-- Name: categories categories_insert; Type: TRIGGER; Schema: public; Owner: dan
--

CREATE TRIGGER categories_insert AFTER INSERT OR UPDATE ON public.categories FOR EACH ROW EXECUTE FUNCTION public.categories_insert_update();


--
-- Name: categories categories_new; Type: TRIGGER; Schema: public; Owner: dan
--

CREATE TRIGGER categories_new AFTER INSERT ON public.categories FOR EACH ROW EXECUTE FUNCTION public.categories_new();


--
-- Name: ports check_last_commit_id; Type: TRIGGER; Schema: public; Owner: dan
--

CREATE TRIGGER check_last_commit_id BEFORE UPDATE ON public.ports FOR EACH ROW EXECUTE FUNCTION public.check_last_commit_id();


--
-- Name: element check_parent; Type: TRIGGER; Schema: public; Owner: dan
--

CREATE TRIGGER check_parent BEFORE INSERT OR UPDATE ON public.element FOR EACH ROW EXECUTE FUNCTION public.check_parent();


--
-- Name: element check_siblings; Type: TRIGGER; Schema: public; Owner: dan
--

CREATE TRIGGER check_siblings AFTER INSERT OR UPDATE ON public.element FOR EACH ROW EXECUTE FUNCTION public.check_siblings();


--
-- Name: commit_log commit_log_delete_check; Type: TRIGGER; Schema: public; Owner: dan
--

CREATE TRIGGER commit_log_delete_check BEFORE DELETE ON public.commit_log FOR EACH ROW EXECUTE FUNCTION public.commit_log_delete_check();


--
-- Name: commit_log commit_log_insert; Type: TRIGGER; Schema: public; Owner: dan
--

CREATE TRIGGER commit_log_insert AFTER INSERT ON public.commit_log FOR EACH ROW EXECUTE FUNCTION public.commit_log_insert();


--
-- Name: commit_log_ports commit_log_ports_insert; Type: TRIGGER; Schema: public; Owner: dan
--

CREATE TRIGGER commit_log_ports_insert AFTER INSERT ON public.commit_log_ports FOR EACH ROW EXECUTE FUNCTION public.commit_log_ports_insert();


--
-- Name: commit_log_ports_vuxml commit_log_ports_vuxml_clear_cache; Type: TRIGGER; Schema: public; Owner: dan
--

CREATE TRIGGER commit_log_ports_vuxml_clear_cache AFTER INSERT OR DELETE OR UPDATE ON public.commit_log_ports_vuxml FOR EACH ROW EXECUTE FUNCTION public.commit_log_ports_vuxml_clear_cache();


--
-- Name: commit_log commit_log_update; Type: TRIGGER; Schema: public; Owner: dan
--

CREATE TRIGGER commit_log_update AFTER UPDATE ON public.commit_log FOR EACH ROW EXECUTE FUNCTION public.commit_log_update();


--
-- Name: commit_log_elements element_delete_check; Type: TRIGGER; Schema: public; Owner: dan
--

CREATE TRIGGER element_delete_check BEFORE INSERT OR UPDATE ON public.commit_log_elements FOR EACH ROW EXECUTE FUNCTION public.element_delete_check();


--
-- Name: element element_pathname_insert; Type: TRIGGER; Schema: public; Owner: dan
--

CREATE TRIGGER element_pathname_insert AFTER INSERT ON public.element FOR EACH ROW EXECUTE FUNCTION public.element_pathname_insert();


--
-- Name: element element_pathname_update; Type: TRIGGER; Schema: public; Owner: dan
--

CREATE TRIGGER element_pathname_update AFTER UPDATE ON public.element FOR EACH ROW EXECUTE FUNCTION public.element_pathname_update();


--
-- Name: element element_ports_status; Type: TRIGGER; Schema: public; Owner: dan
--

CREATE TRIGGER element_ports_status AFTER UPDATE ON public.element FOR EACH ROW EXECUTE FUNCTION public.element_ports_status();


--
-- Name: element id_change; Type: TRIGGER; Schema: public; Owner: dan
--

CREATE TRIGGER id_change BEFORE UPDATE ON public.element FOR EACH ROW EXECUTE FUNCTION public.id_change();


--
-- Name: element on_delete_remove_children; Type: TRIGGER; Schema: public; Owner: dan
--

CREATE TRIGGER on_delete_remove_children BEFORE DELETE ON public.element FOR EACH ROW EXECUTE FUNCTION public.on_delete_remove_children();


--
-- Name: element parent_must_be_directory; Type: TRIGGER; Schema: public; Owner: dan
--

CREATE TRIGGER parent_must_be_directory BEFORE INSERT OR UPDATE ON public.element FOR EACH ROW EXECUTE FUNCTION public.parent_must_be_directory();


--
-- Name: ports port_dependencies_clear_cache; Type: TRIGGER; Schema: public; Owner: dan
--

CREATE TRIGGER port_dependencies_clear_cache AFTER INSERT OR DELETE OR UPDATE ON public.ports FOR EACH ROW EXECUTE FUNCTION public.port_dependencies_clear_cache();


--
-- Name: port_dependencies port_dependencies_delete_clear_cache; Type: TRIGGER; Schema: public; Owner: dan
--

CREATE TRIGGER port_dependencies_delete_clear_cache AFTER DELETE ON public.port_dependencies FOR EACH ROW EXECUTE FUNCTION public.port_dependencies_delete_clear_cache();


--
-- Name: port_dependencies port_dependencies_insert_clear_cache; Type: TRIGGER; Schema: public; Owner: dan
--

CREATE TRIGGER port_dependencies_insert_clear_cache AFTER INSERT ON public.port_dependencies FOR EACH ROW EXECUTE FUNCTION public.port_dependencies_insert_clear_cache();


--
-- Name: ports ports_clear_cache; Type: TRIGGER; Schema: public; Owner: dan
--

CREATE TRIGGER ports_clear_cache AFTER UPDATE ON public.ports FOR EACH ROW EXECUTE FUNCTION public.ports_clear_cache();


--
-- Name: ports ports_conflicts_insert; Type: TRIGGER; Schema: public; Owner: dan
--

CREATE TRIGGER ports_conflicts_insert AFTER INSERT ON public.ports FOR EACH ROW EXECUTE FUNCTION public.ports_conflicts_set();


--
-- Name: ports ports_conflicts_update; Type: TRIGGER; Schema: public; Owner: dan
--

CREATE TRIGGER ports_conflicts_update AFTER UPDATE ON public.ports FOR EACH ROW EXECUTE FUNCTION public.ports_conflicts_set();


--
-- Name: ports ports_ports_categories; Type: TRIGGER; Schema: public; Owner: dan
--

CREATE TRIGGER ports_ports_categories AFTER INSERT OR UPDATE ON public.ports FOR EACH ROW EXECUTE FUNCTION public.ports_categories_set();


--
-- Name: ports ports_status; Type: TRIGGER; Schema: public; Owner: dan
--

CREATE TRIGGER ports_status BEFORE INSERT OR UPDATE ON public.ports FOR EACH ROW EXECUTE FUNCTION public.ports_status();


--
-- Name: ports_vulnerable ports_vulnerable_delete_clear_cache; Type: TRIGGER; Schema: public; Owner: dan
--

CREATE TRIGGER ports_vulnerable_delete_clear_cache AFTER DELETE ON public.ports_vulnerable FOR EACH ROW EXECUTE FUNCTION public.ports_vulnerable_delete_clear_cache();


--
-- Name: security_notice security_notice_audit; Type: TRIGGER; Schema: public; Owner: dan
--

CREATE TRIGGER security_notice_audit BEFORE UPDATE ON public.security_notice FOR EACH ROW EXECUTE FUNCTION public.security_notice_audit();


--
-- Name: users user_email_change; Type: TRIGGER; Schema: public; Owner: dan
--

CREATE TRIGGER user_email_change AFTER UPDATE ON public.users FOR EACH ROW EXECUTE FUNCTION public.user_email_change();


--
-- Name: user_password_reset user_password_reset_token; Type: TRIGGER; Schema: public; Owner: dan
--

CREATE TRIGGER user_password_reset_token BEFORE INSERT ON public.user_password_reset FOR EACH ROW EXECUTE FUNCTION public.user_password_reset_token();


--
-- Name: users user_watch_list_create; Type: TRIGGER; Schema: public; Owner: dan
--

CREATE TRIGGER user_watch_list_create AFTER INSERT ON public.users FOR EACH ROW EXECUTE FUNCTION public.user_watch_list_create();


--
-- Name: system_branch_element_revision version_revision_uniqueness; Type: TRIGGER; Schema: public; Owner: dan
--

CREATE TRIGGER version_revision_uniqueness BEFORE INSERT OR UPDATE ON public.system_branch_element_revision FOR EACH ROW EXECUTE FUNCTION public.version_revision_uniqueness();


--
-- Name: watch_list_element watch_list_element_delete; Type: TRIGGER; Schema: public; Owner: dan
--

CREATE TRIGGER watch_list_element_delete BEFORE DELETE ON public.watch_list_element FOR EACH ROW EXECUTE FUNCTION public.watch_list_element_delete();


--
-- Name: watch_list_element watch_list_element_insert; Type: TRIGGER; Schema: public; Owner: dan
--

CREATE TRIGGER watch_list_element_insert AFTER INSERT ON public.watch_list_element FOR EACH ROW EXECUTE FUNCTION public.watch_list_element_insert();


--
-- Name: vuxml_references $1; Type: FK CONSTRAINT; Schema: public; Owner: dan
--

ALTER TABLE ONLY public.vuxml_references
    ADD CONSTRAINT "$1" FOREIGN KEY (vuxml_id) REFERENCES public.vuxml(id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: watch_list $1; Type: FK CONSTRAINT; Schema: public; Owner: dan
--

ALTER TABLE ONLY public.watch_list
    ADD CONSTRAINT "$1" FOREIGN KEY (user_id) REFERENCES public.users(id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: user_confirmations $1; Type: FK CONSTRAINT; Schema: public; Owner: dan
--

ALTER TABLE ONLY public.user_confirmations
    ADD CONSTRAINT "$1" FOREIGN KEY (user_id) REFERENCES public.users(id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: vuxml_affected $1; Type: FK CONSTRAINT; Schema: public; Owner: dan
--

ALTER TABLE ONLY public.vuxml_affected
    ADD CONSTRAINT "$1" FOREIGN KEY (vuxml_id) REFERENCES public.vuxml(id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: vuxml_names $1; Type: FK CONSTRAINT; Schema: public; Owner: dan
--

ALTER TABLE ONLY public.vuxml_names
    ADD CONSTRAINT "$1" FOREIGN KEY (vuxml_affected_id) REFERENCES public.vuxml_affected(id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: vuxml_ranges $1; Type: FK CONSTRAINT; Schema: public; Owner: dan
--

ALTER TABLE ONLY public.vuxml_ranges
    ADD CONSTRAINT "$1" FOREIGN KEY (vuxml_affected_id) REFERENCES public.vuxml_affected(id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: security_notice $1; Type: FK CONSTRAINT; Schema: public; Owner: dan
--

ALTER TABLE ONLY public.security_notice
    ADD CONSTRAINT "$1" FOREIGN KEY (user_id) REFERENCES public.users(id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: committer_notify $1; Type: FK CONSTRAINT; Schema: public; Owner: dan
--

ALTER TABLE ONLY public.committer_notify
    ADD CONSTRAINT "$1" FOREIGN KEY (user_id) REFERENCES public.users(id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: security_notice_audit $1; Type: FK CONSTRAINT; Schema: public; Owner: dan
--

ALTER TABLE ONLY public.security_notice_audit
    ADD CONSTRAINT "$1" FOREIGN KEY (security_notice_id) REFERENCES public.security_notice(id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: users $1; Type: FK CONSTRAINT; Schema: public; Owner: dan
--

ALTER TABLE ONLY public.users
    ADD CONSTRAINT "$1" FOREIGN KEY (watch_notice_id) REFERENCES public.watch_notice(id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: report_subscriptions $1; Type: FK CONSTRAINT; Schema: public; Owner: dan
--

ALTER TABLE ONLY public.report_subscriptions
    ADD CONSTRAINT "$1" FOREIGN KEY (report_frequency_id) REFERENCES public.report_frequency(id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: system_branch $1; Type: FK CONSTRAINT; Schema: public; Owner: dan
--

ALTER TABLE ONLY public.system_branch
    ADD CONSTRAINT "$1" FOREIGN KEY (system_id) REFERENCES public.system(id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: user_tasks $1; Type: FK CONSTRAINT; Schema: public; Owner: dan
--

ALTER TABLE ONLY public.user_tasks
    ADD CONSTRAINT "$1" FOREIGN KEY (user_id) REFERENCES public.users(id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: report_log $1; Type: FK CONSTRAINT; Schema: public; Owner: dan
--

ALTER TABLE ONLY public.report_log
    ADD CONSTRAINT "$1" FOREIGN KEY (frequency_id) REFERENCES public.report_frequency(id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: commit_log_ports_vuxml $1; Type: FK CONSTRAINT; Schema: public; Owner: dan
--

ALTER TABLE ONLY public.commit_log_ports_vuxml
    ADD CONSTRAINT "$1" FOREIGN KEY (vuxml_id) REFERENCES public.vuxml(id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: categories $1; Type: FK CONSTRAINT; Schema: public; Owner: dan
--

ALTER TABLE ONLY public.categories
    ADD CONSTRAINT "$1" FOREIGN KEY (element_id) REFERENCES public.element(id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: element $1; Type: FK CONSTRAINT; Schema: public; Owner: dan
--

ALTER TABLE ONLY public.element
    ADD CONSTRAINT "$1" FOREIGN KEY (parent_id) REFERENCES public.element(id) ON UPDATE RESTRICT ON DELETE CASCADE;


--
-- Name: system_branch_element_revision $1; Type: FK CONSTRAINT; Schema: public; Owner: dan
--

ALTER TABLE ONLY public.system_branch_element_revision
    ADD CONSTRAINT "$1" FOREIGN KEY (element_id, revision_name) REFERENCES public.element_revision(element_id, revision_name) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: watch_list_element $1; Type: FK CONSTRAINT; Schema: public; Owner: dan
--

ALTER TABLE ONLY public.watch_list_element
    ADD CONSTRAINT "$1" FOREIGN KEY (element_id) REFERENCES public.element(id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: watch_list_staging $1; Type: FK CONSTRAINT; Schema: public; Owner: dan
--

ALTER TABLE ONLY public.watch_list_staging
    ADD CONSTRAINT "$1" FOREIGN KEY (element_id) REFERENCES public.element(id) ON UPDATE CASCADE ON DELETE SET NULL;


--
-- Name: commit_log $1; Type: FK CONSTRAINT; Schema: public; Owner: dan
--

ALTER TABLE ONLY public.commit_log
    ADD CONSTRAINT "$1" FOREIGN KEY (system_id) REFERENCES public.system(id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: commit_log_ports_ignore $1; Type: FK CONSTRAINT; Schema: public; Owner: dan
--

ALTER TABLE ONLY public.commit_log_ports_ignore
    ADD CONSTRAINT "$1" FOREIGN KEY (port_id) REFERENCES public.ports(id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: ports $1; Type: FK CONSTRAINT; Schema: public; Owner: dan
--

ALTER TABLE ONLY public.ports
    ADD CONSTRAINT "$1" FOREIGN KEY (category_id) REFERENCES public.categories(id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: ports_categories $1; Type: FK CONSTRAINT; Schema: public; Owner: dan
--

ALTER TABLE ONLY public.ports_categories
    ADD CONSTRAINT "$1" FOREIGN KEY (port_id) REFERENCES public.ports(id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: commit_log_port_elements $1; Type: FK CONSTRAINT; Schema: public; Owner: dan
--

ALTER TABLE ONLY public.commit_log_port_elements
    ADD CONSTRAINT "$1" FOREIGN KEY (commit_log_id) REFERENCES public.commit_log(id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: ports_moved $1; Type: FK CONSTRAINT; Schema: public; Owner: dan
--

ALTER TABLE ONLY public.ports_moved
    ADD CONSTRAINT "$1" FOREIGN KEY (from_port_id) REFERENCES public.ports(id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: ports_updating_ports_xref $1; Type: FK CONSTRAINT; Schema: public; Owner: dan
--

ALTER TABLE ONLY public.ports_updating_ports_xref
    ADD CONSTRAINT "$1" FOREIGN KEY (port_id) REFERENCES public.ports(id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: commit_log_ports $1; Type: FK CONSTRAINT; Schema: public; Owner: dan
--

ALTER TABLE ONLY public.commit_log_ports
    ADD CONSTRAINT "$1" FOREIGN KEY (commit_log_id) REFERENCES public.commit_log(id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: ports_vulnerable $1; Type: FK CONSTRAINT; Schema: public; Owner: dan
--

ALTER TABLE ONLY public.ports_vulnerable
    ADD CONSTRAINT "$1" FOREIGN KEY (port_id) REFERENCES public.ports(id) ON UPDATE RESTRICT ON DELETE CASCADE;


--
-- Name: element_revision $1; Type: FK CONSTRAINT; Schema: public; Owner: dan
--

ALTER TABLE ONLY public.element_revision
    ADD CONSTRAINT "$1" FOREIGN KEY (element_id) REFERENCES public.element(id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: commit_log_elements $1; Type: FK CONSTRAINT; Schema: public; Owner: dan
--

ALTER TABLE ONLY public.commit_log_elements
    ADD CONSTRAINT "$1" FOREIGN KEY (commit_log_id) REFERENCES public.commit_log(id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: commits_latest_ports $1; Type: FK CONSTRAINT; Schema: public; Owner: dan
--

ALTER TABLE ONLY public.commits_latest_ports
    ADD CONSTRAINT "$1" FOREIGN KEY (commit_log_id) REFERENCES public.commit_log(id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: latest_commits $1; Type: FK CONSTRAINT; Schema: public; Owner: dan
--

ALTER TABLE ONLY public.latest_commits
    ADD CONSTRAINT "$1" FOREIGN KEY (commit_log_id) REFERENCES public.commit_log(id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: latest_commits_ports $1; Type: FK CONSTRAINT; Schema: public; Owner: dan
--

ALTER TABLE ONLY public.latest_commits_ports
    ADD CONSTRAINT "$1" FOREIGN KEY (commit_log_id) REFERENCES public.commit_log(id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: users $2; Type: FK CONSTRAINT; Schema: public; Owner: dan
--

ALTER TABLE ONLY public.users
    ADD CONSTRAINT "$2" FOREIGN KEY (last_watch_list_chosen) REFERENCES public.watch_list(id) ON UPDATE CASCADE ON DELETE SET NULL;


--
-- Name: ports_updating_ports_xref $2; Type: FK CONSTRAINT; Schema: public; Owner: dan
--

ALTER TABLE ONLY public.ports_updating_ports_xref
    ADD CONSTRAINT "$2" FOREIGN KEY (ports_updating_id) REFERENCES public.ports_updating(id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: ports_categories $2; Type: FK CONSTRAINT; Schema: public; Owner: dan
--

ALTER TABLE ONLY public.ports_categories
    ADD CONSTRAINT "$2" FOREIGN KEY (category_id) REFERENCES public.categories(id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: report_log $2; Type: FK CONSTRAINT; Schema: public; Owner: dan
--

ALTER TABLE ONLY public.report_log
    ADD CONSTRAINT "$2" FOREIGN KEY (report_id) REFERENCES public.reports(id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: user_tasks $2; Type: FK CONSTRAINT; Schema: public; Owner: dan
--

ALTER TABLE ONLY public.user_tasks
    ADD CONSTRAINT "$2" FOREIGN KEY (task_id) REFERENCES public.tasks(id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: security_notice_audit $2; Type: FK CONSTRAINT; Schema: public; Owner: dan
--

ALTER TABLE ONLY public.security_notice_audit
    ADD CONSTRAINT "$2" FOREIGN KEY (security_notice_status_id) REFERENCES public.security_notice_status(id) ON UPDATE RESTRICT ON DELETE RESTRICT;


--
-- Name: report_subscriptions $2; Type: FK CONSTRAINT; Schema: public; Owner: dan
--

ALTER TABLE ONLY public.report_subscriptions
    ADD CONSTRAINT "$2" FOREIGN KEY (report_id) REFERENCES public.reports(id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: watch_list_element $2; Type: FK CONSTRAINT; Schema: public; Owner: dan
--

ALTER TABLE ONLY public.watch_list_element
    ADD CONSTRAINT "$2" FOREIGN KEY (watch_list_id) REFERENCES public.watch_list(id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: system_branch_element_revision $2; Type: FK CONSTRAINT; Schema: public; Owner: dan
--

ALTER TABLE ONLY public.system_branch_element_revision
    ADD CONSTRAINT "$2" FOREIGN KEY (system_branch_id) REFERENCES public.system_branch(id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: commit_log_port_elements $2; Type: FK CONSTRAINT; Schema: public; Owner: dan
--

ALTER TABLE ONLY public.commit_log_port_elements
    ADD CONSTRAINT "$2" FOREIGN KEY (commit_log_element_id) REFERENCES public.commit_log_elements(id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: commit_log_ports $2; Type: FK CONSTRAINT; Schema: public; Owner: dan
--

ALTER TABLE ONLY public.commit_log_ports
    ADD CONSTRAINT "$2" FOREIGN KEY (port_id) REFERENCES public.ports(id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: commit_log_ports_vuxml $2; Type: FK CONSTRAINT; Schema: public; Owner: dan
--

ALTER TABLE ONLY public.commit_log_ports_vuxml
    ADD CONSTRAINT "$2" FOREIGN KEY (port_id) REFERENCES public.ports(id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: commit_log_elements $2; Type: FK CONSTRAINT; Schema: public; Owner: dan
--

ALTER TABLE ONLY public.commit_log_elements
    ADD CONSTRAINT "$2" FOREIGN KEY (element_id, revision_name) REFERENCES public.element_revision(element_id, revision_name) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: ports $2; Type: FK CONSTRAINT; Schema: public; Owner: dan
--

ALTER TABLE ONLY public.ports
    ADD CONSTRAINT "$2" FOREIGN KEY (element_id) REFERENCES public.element(id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: ports_moved $2; Type: FK CONSTRAINT; Schema: public; Owner: dan
--

ALTER TABLE ONLY public.ports_moved
    ADD CONSTRAINT "$2" FOREIGN KEY (to_port_id) REFERENCES public.ports(id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: commit_log_ports_ignore $2; Type: FK CONSTRAINT; Schema: public; Owner: dan
--

ALTER TABLE ONLY public.commit_log_ports_ignore
    ADD CONSTRAINT "$2" FOREIGN KEY (commit_log_id) REFERENCES public.commit_log(id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: security_notice $2; Type: FK CONSTRAINT; Schema: public; Owner: dan
--

ALTER TABLE ONLY public.security_notice
    ADD CONSTRAINT "$2" FOREIGN KEY (commit_log_id) REFERENCES public.commit_log(id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: report_subscriptions $3; Type: FK CONSTRAINT; Schema: public; Owner: dan
--

ALTER TABLE ONLY public.report_subscriptions
    ADD CONSTRAINT "$3" FOREIGN KEY (user_id) REFERENCES public.users(id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: security_notice $3; Type: FK CONSTRAINT; Schema: public; Owner: dan
--

ALTER TABLE ONLY public.security_notice
    ADD CONSTRAINT "$3" FOREIGN KEY (security_notice_status_id) REFERENCES public.security_notice_status(id) ON UPDATE RESTRICT ON DELETE RESTRICT;


--
-- Name: commit_log_port_elements $3; Type: FK CONSTRAINT; Schema: public; Owner: dan
--

ALTER TABLE ONLY public.commit_log_port_elements
    ADD CONSTRAINT "$3" FOREIGN KEY (port_id) REFERENCES public.ports(id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: commit_log_ports_vuxml $3; Type: FK CONSTRAINT; Schema: public; Owner: dan
--

ALTER TABLE ONLY public.commit_log_ports_vuxml
    ADD CONSTRAINT "$3" FOREIGN KEY (commit_log_id) REFERENCES public.commit_log(id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: cache_clearing_ports cache_clearing_ports_port_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: dan
--

ALTER TABLE ONLY public.cache_clearing_ports
    ADD CONSTRAINT cache_clearing_ports_port_id_fkey FOREIGN KEY (port_id) REFERENCES public.ports(id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: category_stats category_stats_category_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: dan
--

ALTER TABLE ONLY public.category_stats
    ADD CONSTRAINT category_stats_category_id_fkey FOREIGN KEY (category_id) REFERENCES public.categories(id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: commit_log_branches commit_log_branch_branch_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: dan
--

ALTER TABLE ONLY public.commit_log_branches
    ADD CONSTRAINT commit_log_branch_branch_id_fkey FOREIGN KEY (branch_id) REFERENCES public.system_branch(id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: commit_log_branch_delete_me commit_log_branch_branch_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: dan
--

ALTER TABLE ONLY public.commit_log_branch_delete_me
    ADD CONSTRAINT commit_log_branch_branch_id_fkey FOREIGN KEY (branch_id) REFERENCES public.system_branch(id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: commit_log_branch_delete_me commit_log_branch_commit_log_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: dan
--

ALTER TABLE ONLY public.commit_log_branch_delete_me
    ADD CONSTRAINT commit_log_branch_commit_log_id_fkey FOREIGN KEY (commit_log_id) REFERENCES public.commit_log(id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: commit_log_branches commit_log_branch_commit_log_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: dan
--

ALTER TABLE ONLY public.commit_log_branches
    ADD CONSTRAINT commit_log_branch_commit_log_id_fkey FOREIGN KEY (commit_log_id) REFERENCES public.commit_log(id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: commit_log_ports_elements commit_log_ports_elements_commit_log_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: dan
--

ALTER TABLE ONLY public.commit_log_ports_elements
    ADD CONSTRAINT commit_log_ports_elements_commit_log_id_fkey FOREIGN KEY (commit_log_id) REFERENCES public.commit_log(id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: commit_log_ports_elements commit_log_ports_elements_element_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: dan
--

ALTER TABLE ONLY public.commit_log_ports_elements
    ADD CONSTRAINT commit_log_ports_elements_element_id_fkey FOREIGN KEY (element_id) REFERENCES public.element(id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: commit_log commit_log_repo_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: dan
--

ALTER TABLE ONLY public.commit_log
    ADD CONSTRAINT commit_log_repo_id_fkey FOREIGN KEY (repo_id) REFERENCES public.repo(id) ON UPDATE CASCADE ON DELETE RESTRICT;


--
-- Name: commits_flagged commits_flagged_commit_log_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: dan
--

ALTER TABLE ONLY public.commits_flagged
    ADD CONSTRAINT commits_flagged_commit_log_id_fkey FOREIGN KEY (commit_log_id) REFERENCES public.commit_log(id);


--
-- Name: commits_flagged commits_flagged_user_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: dan
--

ALTER TABLE ONLY public.commits_flagged
    ADD CONSTRAINT commits_flagged_user_id_fkey FOREIGN KEY (user_id) REFERENCES public.users(id);


--
-- Name: design_results design_results_user_id; Type: FK CONSTRAINT; Schema: public; Owner: dan
--

ALTER TABLE ONLY public.design_results
    ADD CONSTRAINT design_results_user_id FOREIGN KEY (user_id) REFERENCES public.users(id);


--
-- Name: element_pathname element_pathname_element_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: dan
--

ALTER TABLE ONLY public.element_pathname
    ADD CONSTRAINT element_pathname_element_id_fkey FOREIGN KEY (element_id) REFERENCES public.element(id) ON DELETE CASCADE;


--
-- Name: package_flavors package_flavors_flavor_id_fk; Type: FK CONSTRAINT; Schema: public; Owner: dan
--

ALTER TABLE ONLY public.package_flavors
    ADD CONSTRAINT package_flavors_flavor_id_fk FOREIGN KEY (flavor_id) REFERENCES public.flavors(id);


--
-- Name: package_flavors package_flavors_port_id_fk; Type: FK CONSTRAINT; Schema: public; Owner: dan
--

ALTER TABLE ONLY public.package_flavors
    ADD CONSTRAINT package_flavors_port_id_fk FOREIGN KEY (port_id) REFERENCES public.ports(id);


--
-- Name: port_dependencies port_dependencies_port_id_dependent_upon_fkey; Type: FK CONSTRAINT; Schema: public; Owner: dan
--

ALTER TABLE ONLY public.port_dependencies
    ADD CONSTRAINT port_dependencies_port_id_dependent_upon_fkey FOREIGN KEY (port_id_dependent_upon) REFERENCES public.ports(id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: port_dependencies port_dependencies_port_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: dan
--

ALTER TABLE ONLY public.port_dependencies
    ADD CONSTRAINT port_dependencies_port_id_fkey FOREIGN KEY (port_id) REFERENCES public.ports(id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: ports_conflicts_matches ports_conflicts_matches_conflicts_id; Type: FK CONSTRAINT; Schema: public; Owner: dan
--

ALTER TABLE ONLY public.ports_conflicts_matches
    ADD CONSTRAINT ports_conflicts_matches_conflicts_id FOREIGN KEY (ports_conflicts_id) REFERENCES public.ports_conflicts(id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: ports_conflicts_matches ports_conflicts_matches_port_id; Type: FK CONSTRAINT; Schema: public; Owner: dan
--

ALTER TABLE ONLY public.ports_conflicts_matches
    ADD CONSTRAINT ports_conflicts_matches_port_id FOREIGN KEY (port_id) REFERENCES public.ports(id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: ports_conflicts ports_conflicts_port_id; Type: FK CONSTRAINT; Schema: public; Owner: dan
--

ALTER TABLE ONLY public.ports_conflicts
    ADD CONSTRAINT ports_conflicts_port_id FOREIGN KEY (port_id) REFERENCES public.ports(id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: ports ports_last_commit_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: dan
--

ALTER TABLE ONLY public.ports
    ADD CONSTRAINT ports_last_commit_id_fkey FOREIGN KEY (last_commit_id) REFERENCES public.commit_log(id) ON UPDATE CASCADE ON DELETE SET NULL;


--
-- Name: ports ports_last_commit_id_fkey1; Type: FK CONSTRAINT; Schema: public; Owner: dan
--

ALTER TABLE ONLY public.ports
    ADD CONSTRAINT ports_last_commit_id_fkey1 FOREIGN KEY (last_commit_id) REFERENCES public.commit_log(id) ON UPDATE CASCADE ON DELETE SET NULL;


--
-- Name: sanity_test_failures sanity_test_failures_commit_log_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: dan
--

ALTER TABLE ONLY public.sanity_test_failures
    ADD CONSTRAINT sanity_test_failures_commit_log_id_fkey FOREIGN KEY (commit_log_id) REFERENCES public.commit_log(id) ON UPDATE RESTRICT ON DELETE CASCADE;


--
-- Name: user_password_reset user_password_reset_user_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: dan
--

ALTER TABLE ONLY public.user_password_reset
    ADD CONSTRAINT user_password_reset_user_id_fkey FOREIGN KEY (user_id) REFERENCES public.users(id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: watch_list_staging wls_user; Type: FK CONSTRAINT; Schema: public; Owner: dan
--

ALTER TABLE ONLY public.watch_list_staging
    ADD CONSTRAINT wls_user FOREIGN KEY (user_id) REFERENCES public.users(id) ON DELETE CASCADE;


--
-- Name: SCHEMA public; Type: ACL; Schema: -; Owner: postgres
--

GRANT ALL ON SCHEMA public TO pgsql;


--
-- Name: TYPE mylogincounts_record; Type: ACL; Schema: public; Owner: dan
--

REVOKE ALL ON TYPE public.mylogincounts_record FROM dan;


--
-- Name: TABLE ports_conflicts_matches; Type: ACL; Schema: public; Owner: dan
--

GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE public.ports_conflicts_matches TO commits;
GRANT SELECT ON TABLE public.ports_conflicts_matches TO rsyncer;
GRANT SELECT ON TABLE public.ports_conflicts_matches TO www;


--
-- Name: TABLE pg_aggregate; Type: ACL; Schema: pg_catalog; Owner: postgres
--

GRANT SELECT ON TABLE pg_catalog.pg_aggregate TO rsyncer;


--
-- Name: TABLE pg_am; Type: ACL; Schema: pg_catalog; Owner: postgres
--

GRANT SELECT ON TABLE pg_catalog.pg_am TO rsyncer;


--
-- Name: TABLE pg_amop; Type: ACL; Schema: pg_catalog; Owner: postgres
--

GRANT SELECT ON TABLE pg_catalog.pg_amop TO rsyncer;


--
-- Name: TABLE pg_amproc; Type: ACL; Schema: pg_catalog; Owner: postgres
--

GRANT SELECT ON TABLE pg_catalog.pg_amproc TO rsyncer;


--
-- Name: TABLE pg_attrdef; Type: ACL; Schema: pg_catalog; Owner: postgres
--

GRANT SELECT ON TABLE pg_catalog.pg_attrdef TO rsyncer;


--
-- Name: TABLE pg_attribute; Type: ACL; Schema: pg_catalog; Owner: postgres
--

GRANT SELECT ON TABLE pg_catalog.pg_attribute TO rsyncer;


--
-- Name: TABLE pg_auth_members; Type: ACL; Schema: pg_catalog; Owner: postgres
--

GRANT SELECT ON TABLE pg_catalog.pg_auth_members TO rsyncer;


--
-- Name: TABLE pg_authid; Type: ACL; Schema: pg_catalog; Owner: postgres
--

GRANT SELECT ON TABLE pg_catalog.pg_authid TO rsyncer;


--
-- Name: TABLE pg_available_extension_versions; Type: ACL; Schema: pg_catalog; Owner: postgres
--

GRANT SELECT ON TABLE pg_catalog.pg_available_extension_versions TO rsyncer;


--
-- Name: TABLE pg_available_extensions; Type: ACL; Schema: pg_catalog; Owner: postgres
--

GRANT SELECT ON TABLE pg_catalog.pg_available_extensions TO rsyncer;


--
-- Name: TABLE pg_cast; Type: ACL; Schema: pg_catalog; Owner: postgres
--

GRANT SELECT ON TABLE pg_catalog.pg_cast TO rsyncer;


--
-- Name: TABLE pg_class; Type: ACL; Schema: pg_catalog; Owner: postgres
--

GRANT SELECT ON TABLE pg_catalog.pg_class TO rsyncer;


--
-- Name: TABLE pg_collation; Type: ACL; Schema: pg_catalog; Owner: postgres
--

GRANT SELECT ON TABLE pg_catalog.pg_collation TO rsyncer;


--
-- Name: TABLE pg_config; Type: ACL; Schema: pg_catalog; Owner: postgres
--

GRANT SELECT ON TABLE pg_catalog.pg_config TO rsyncer;


--
-- Name: TABLE pg_constraint; Type: ACL; Schema: pg_catalog; Owner: postgres
--

GRANT SELECT ON TABLE pg_catalog.pg_constraint TO rsyncer;


--
-- Name: TABLE pg_conversion; Type: ACL; Schema: pg_catalog; Owner: postgres
--

GRANT SELECT ON TABLE pg_catalog.pg_conversion TO rsyncer;


--
-- Name: TABLE pg_cursors; Type: ACL; Schema: pg_catalog; Owner: postgres
--

GRANT SELECT ON TABLE pg_catalog.pg_cursors TO rsyncer;


--
-- Name: TABLE pg_database; Type: ACL; Schema: pg_catalog; Owner: postgres
--

GRANT SELECT ON TABLE pg_catalog.pg_database TO rsyncer;


--
-- Name: TABLE pg_db_role_setting; Type: ACL; Schema: pg_catalog; Owner: postgres
--

GRANT SELECT ON TABLE pg_catalog.pg_db_role_setting TO rsyncer;


--
-- Name: TABLE pg_default_acl; Type: ACL; Schema: pg_catalog; Owner: postgres
--

GRANT SELECT ON TABLE pg_catalog.pg_default_acl TO rsyncer;


--
-- Name: TABLE pg_depend; Type: ACL; Schema: pg_catalog; Owner: postgres
--

GRANT SELECT ON TABLE pg_catalog.pg_depend TO rsyncer;


--
-- Name: TABLE pg_description; Type: ACL; Schema: pg_catalog; Owner: postgres
--

GRANT SELECT ON TABLE pg_catalog.pg_description TO rsyncer;


--
-- Name: TABLE pg_enum; Type: ACL; Schema: pg_catalog; Owner: postgres
--

GRANT SELECT ON TABLE pg_catalog.pg_enum TO rsyncer;


--
-- Name: TABLE pg_event_trigger; Type: ACL; Schema: pg_catalog; Owner: postgres
--

GRANT SELECT ON TABLE pg_catalog.pg_event_trigger TO rsyncer;


--
-- Name: TABLE pg_extension; Type: ACL; Schema: pg_catalog; Owner: postgres
--

GRANT SELECT ON TABLE pg_catalog.pg_extension TO rsyncer;


--
-- Name: TABLE pg_file_settings; Type: ACL; Schema: pg_catalog; Owner: postgres
--

GRANT SELECT ON TABLE pg_catalog.pg_file_settings TO rsyncer;


--
-- Name: TABLE pg_foreign_data_wrapper; Type: ACL; Schema: pg_catalog; Owner: postgres
--

GRANT SELECT ON TABLE pg_catalog.pg_foreign_data_wrapper TO rsyncer;


--
-- Name: TABLE pg_foreign_server; Type: ACL; Schema: pg_catalog; Owner: postgres
--

GRANT SELECT ON TABLE pg_catalog.pg_foreign_server TO rsyncer;


--
-- Name: TABLE pg_foreign_table; Type: ACL; Schema: pg_catalog; Owner: postgres
--

GRANT SELECT ON TABLE pg_catalog.pg_foreign_table TO rsyncer;


--
-- Name: TABLE pg_group; Type: ACL; Schema: pg_catalog; Owner: postgres
--

GRANT SELECT ON TABLE pg_catalog.pg_group TO rsyncer;


--
-- Name: TABLE pg_hba_file_rules; Type: ACL; Schema: pg_catalog; Owner: postgres
--

GRANT SELECT ON TABLE pg_catalog.pg_hba_file_rules TO rsyncer;


--
-- Name: TABLE pg_index; Type: ACL; Schema: pg_catalog; Owner: postgres
--

GRANT SELECT ON TABLE pg_catalog.pg_index TO rsyncer;


--
-- Name: TABLE pg_indexes; Type: ACL; Schema: pg_catalog; Owner: postgres
--

GRANT SELECT ON TABLE pg_catalog.pg_indexes TO rsyncer;


--
-- Name: TABLE pg_inherits; Type: ACL; Schema: pg_catalog; Owner: postgres
--

GRANT SELECT ON TABLE pg_catalog.pg_inherits TO rsyncer;


--
-- Name: TABLE pg_init_privs; Type: ACL; Schema: pg_catalog; Owner: postgres
--

GRANT SELECT ON TABLE pg_catalog.pg_init_privs TO rsyncer;


--
-- Name: TABLE pg_language; Type: ACL; Schema: pg_catalog; Owner: postgres
--

GRANT SELECT ON TABLE pg_catalog.pg_language TO rsyncer;


--
-- Name: TABLE pg_largeobject; Type: ACL; Schema: pg_catalog; Owner: postgres
--

GRANT SELECT ON TABLE pg_catalog.pg_largeobject TO rsyncer;


--
-- Name: TABLE pg_largeobject_metadata; Type: ACL; Schema: pg_catalog; Owner: postgres
--

GRANT SELECT ON TABLE pg_catalog.pg_largeobject_metadata TO rsyncer;


--
-- Name: TABLE pg_locks; Type: ACL; Schema: pg_catalog; Owner: postgres
--

GRANT SELECT ON TABLE pg_catalog.pg_locks TO rsyncer;


--
-- Name: TABLE pg_matviews; Type: ACL; Schema: pg_catalog; Owner: postgres
--

GRANT SELECT ON TABLE pg_catalog.pg_matviews TO rsyncer;


--
-- Name: TABLE pg_namespace; Type: ACL; Schema: pg_catalog; Owner: postgres
--

GRANT SELECT ON TABLE pg_catalog.pg_namespace TO rsyncer;


--
-- Name: TABLE pg_opclass; Type: ACL; Schema: pg_catalog; Owner: postgres
--

GRANT SELECT ON TABLE pg_catalog.pg_opclass TO rsyncer;


--
-- Name: TABLE pg_operator; Type: ACL; Schema: pg_catalog; Owner: postgres
--

GRANT SELECT ON TABLE pg_catalog.pg_operator TO rsyncer;


--
-- Name: TABLE pg_opfamily; Type: ACL; Schema: pg_catalog; Owner: postgres
--

GRANT SELECT ON TABLE pg_catalog.pg_opfamily TO rsyncer;


--
-- Name: TABLE pg_partitioned_table; Type: ACL; Schema: pg_catalog; Owner: postgres
--

GRANT SELECT ON TABLE pg_catalog.pg_partitioned_table TO rsyncer;


--
-- Name: TABLE pg_pltemplate; Type: ACL; Schema: pg_catalog; Owner: postgres
--

GRANT SELECT ON TABLE pg_catalog.pg_pltemplate TO rsyncer;


--
-- Name: TABLE pg_policies; Type: ACL; Schema: pg_catalog; Owner: postgres
--

GRANT SELECT ON TABLE pg_catalog.pg_policies TO rsyncer;


--
-- Name: TABLE pg_policy; Type: ACL; Schema: pg_catalog; Owner: postgres
--

GRANT SELECT ON TABLE pg_catalog.pg_policy TO rsyncer;


--
-- Name: TABLE pg_prepared_statements; Type: ACL; Schema: pg_catalog; Owner: postgres
--

GRANT SELECT ON TABLE pg_catalog.pg_prepared_statements TO rsyncer;


--
-- Name: TABLE pg_prepared_xacts; Type: ACL; Schema: pg_catalog; Owner: postgres
--

GRANT SELECT ON TABLE pg_catalog.pg_prepared_xacts TO rsyncer;


--
-- Name: TABLE pg_proc; Type: ACL; Schema: pg_catalog; Owner: postgres
--

GRANT SELECT ON TABLE pg_catalog.pg_proc TO rsyncer;


--
-- Name: TABLE pg_publication; Type: ACL; Schema: pg_catalog; Owner: postgres
--

GRANT SELECT ON TABLE pg_catalog.pg_publication TO rsyncer;


--
-- Name: TABLE pg_publication_rel; Type: ACL; Schema: pg_catalog; Owner: postgres
--

GRANT SELECT ON TABLE pg_catalog.pg_publication_rel TO rsyncer;


--
-- Name: TABLE pg_publication_tables; Type: ACL; Schema: pg_catalog; Owner: postgres
--

GRANT SELECT ON TABLE pg_catalog.pg_publication_tables TO rsyncer;


--
-- Name: TABLE pg_range; Type: ACL; Schema: pg_catalog; Owner: postgres
--

GRANT SELECT ON TABLE pg_catalog.pg_range TO rsyncer;


--
-- Name: TABLE pg_replication_origin; Type: ACL; Schema: pg_catalog; Owner: postgres
--

GRANT SELECT ON TABLE pg_catalog.pg_replication_origin TO rsyncer;


--
-- Name: TABLE pg_replication_origin_status; Type: ACL; Schema: pg_catalog; Owner: postgres
--

GRANT SELECT ON TABLE pg_catalog.pg_replication_origin_status TO rsyncer;


--
-- Name: TABLE pg_replication_slots; Type: ACL; Schema: pg_catalog; Owner: postgres
--

GRANT SELECT ON TABLE pg_catalog.pg_replication_slots TO rsyncer;


--
-- Name: TABLE pg_rewrite; Type: ACL; Schema: pg_catalog; Owner: postgres
--

GRANT SELECT ON TABLE pg_catalog.pg_rewrite TO rsyncer;


--
-- Name: TABLE pg_roles; Type: ACL; Schema: pg_catalog; Owner: postgres
--

GRANT SELECT ON TABLE pg_catalog.pg_roles TO rsyncer;


--
-- Name: TABLE pg_rules; Type: ACL; Schema: pg_catalog; Owner: postgres
--

GRANT SELECT ON TABLE pg_catalog.pg_rules TO rsyncer;


--
-- Name: TABLE pg_seclabel; Type: ACL; Schema: pg_catalog; Owner: postgres
--

GRANT SELECT ON TABLE pg_catalog.pg_seclabel TO rsyncer;


--
-- Name: TABLE pg_seclabels; Type: ACL; Schema: pg_catalog; Owner: postgres
--

GRANT SELECT ON TABLE pg_catalog.pg_seclabels TO rsyncer;


--
-- Name: TABLE pg_sequence; Type: ACL; Schema: pg_catalog; Owner: postgres
--

GRANT SELECT ON TABLE pg_catalog.pg_sequence TO rsyncer;


--
-- Name: TABLE pg_sequences; Type: ACL; Schema: pg_catalog; Owner: postgres
--

GRANT SELECT ON TABLE pg_catalog.pg_sequences TO rsyncer;


--
-- Name: TABLE pg_settings; Type: ACL; Schema: pg_catalog; Owner: postgres
--

GRANT SELECT ON TABLE pg_catalog.pg_settings TO rsyncer;


--
-- Name: TABLE pg_shadow; Type: ACL; Schema: pg_catalog; Owner: postgres
--

GRANT SELECT ON TABLE pg_catalog.pg_shadow TO rsyncer;


--
-- Name: TABLE pg_shdepend; Type: ACL; Schema: pg_catalog; Owner: postgres
--

GRANT SELECT ON TABLE pg_catalog.pg_shdepend TO rsyncer;


--
-- Name: TABLE pg_shdescription; Type: ACL; Schema: pg_catalog; Owner: postgres
--

GRANT SELECT ON TABLE pg_catalog.pg_shdescription TO rsyncer;


--
-- Name: TABLE pg_shseclabel; Type: ACL; Schema: pg_catalog; Owner: postgres
--

GRANT SELECT ON TABLE pg_catalog.pg_shseclabel TO rsyncer;


--
-- Name: TABLE pg_stat_activity; Type: ACL; Schema: pg_catalog; Owner: postgres
--

GRANT SELECT ON TABLE pg_catalog.pg_stat_activity TO rsyncer;


--
-- Name: TABLE pg_stat_all_indexes; Type: ACL; Schema: pg_catalog; Owner: postgres
--

GRANT SELECT ON TABLE pg_catalog.pg_stat_all_indexes TO rsyncer;


--
-- Name: TABLE pg_stat_all_tables; Type: ACL; Schema: pg_catalog; Owner: postgres
--

GRANT SELECT ON TABLE pg_catalog.pg_stat_all_tables TO rsyncer;


--
-- Name: TABLE pg_stat_archiver; Type: ACL; Schema: pg_catalog; Owner: postgres
--

GRANT SELECT ON TABLE pg_catalog.pg_stat_archiver TO rsyncer;


--
-- Name: TABLE pg_stat_bgwriter; Type: ACL; Schema: pg_catalog; Owner: postgres
--

GRANT SELECT ON TABLE pg_catalog.pg_stat_bgwriter TO rsyncer;


--
-- Name: TABLE pg_stat_database; Type: ACL; Schema: pg_catalog; Owner: postgres
--

GRANT SELECT ON TABLE pg_catalog.pg_stat_database TO rsyncer;


--
-- Name: TABLE pg_stat_database_conflicts; Type: ACL; Schema: pg_catalog; Owner: postgres
--

GRANT SELECT ON TABLE pg_catalog.pg_stat_database_conflicts TO rsyncer;


--
-- Name: TABLE pg_stat_progress_vacuum; Type: ACL; Schema: pg_catalog; Owner: postgres
--

GRANT SELECT ON TABLE pg_catalog.pg_stat_progress_vacuum TO rsyncer;


--
-- Name: TABLE pg_stat_replication; Type: ACL; Schema: pg_catalog; Owner: postgres
--

GRANT SELECT ON TABLE pg_catalog.pg_stat_replication TO rsyncer;


--
-- Name: TABLE pg_stat_ssl; Type: ACL; Schema: pg_catalog; Owner: postgres
--

GRANT SELECT ON TABLE pg_catalog.pg_stat_ssl TO rsyncer;


--
-- Name: TABLE pg_stat_subscription; Type: ACL; Schema: pg_catalog; Owner: postgres
--

GRANT SELECT ON TABLE pg_catalog.pg_stat_subscription TO rsyncer;


--
-- Name: TABLE pg_stat_sys_indexes; Type: ACL; Schema: pg_catalog; Owner: postgres
--

GRANT SELECT ON TABLE pg_catalog.pg_stat_sys_indexes TO rsyncer;


--
-- Name: TABLE pg_stat_sys_tables; Type: ACL; Schema: pg_catalog; Owner: postgres
--

GRANT SELECT ON TABLE pg_catalog.pg_stat_sys_tables TO rsyncer;


--
-- Name: TABLE pg_stat_user_functions; Type: ACL; Schema: pg_catalog; Owner: postgres
--

GRANT SELECT ON TABLE pg_catalog.pg_stat_user_functions TO rsyncer;


--
-- Name: TABLE pg_stat_user_indexes; Type: ACL; Schema: pg_catalog; Owner: postgres
--

GRANT SELECT ON TABLE pg_catalog.pg_stat_user_indexes TO rsyncer;


--
-- Name: TABLE pg_stat_user_tables; Type: ACL; Schema: pg_catalog; Owner: postgres
--

GRANT SELECT ON TABLE pg_catalog.pg_stat_user_tables TO rsyncer;


--
-- Name: TABLE pg_stat_wal_receiver; Type: ACL; Schema: pg_catalog; Owner: postgres
--

GRANT SELECT ON TABLE pg_catalog.pg_stat_wal_receiver TO rsyncer;


--
-- Name: TABLE pg_stat_xact_all_tables; Type: ACL; Schema: pg_catalog; Owner: postgres
--

GRANT SELECT ON TABLE pg_catalog.pg_stat_xact_all_tables TO rsyncer;


--
-- Name: TABLE pg_stat_xact_sys_tables; Type: ACL; Schema: pg_catalog; Owner: postgres
--

GRANT SELECT ON TABLE pg_catalog.pg_stat_xact_sys_tables TO rsyncer;


--
-- Name: TABLE pg_stat_xact_user_functions; Type: ACL; Schema: pg_catalog; Owner: postgres
--

GRANT SELECT ON TABLE pg_catalog.pg_stat_xact_user_functions TO rsyncer;


--
-- Name: TABLE pg_stat_xact_user_tables; Type: ACL; Schema: pg_catalog; Owner: postgres
--

GRANT SELECT ON TABLE pg_catalog.pg_stat_xact_user_tables TO rsyncer;


--
-- Name: TABLE pg_statio_all_indexes; Type: ACL; Schema: pg_catalog; Owner: postgres
--

GRANT SELECT ON TABLE pg_catalog.pg_statio_all_indexes TO rsyncer;


--
-- Name: TABLE pg_statio_all_sequences; Type: ACL; Schema: pg_catalog; Owner: postgres
--

GRANT SELECT ON TABLE pg_catalog.pg_statio_all_sequences TO rsyncer;


--
-- Name: TABLE pg_statio_all_tables; Type: ACL; Schema: pg_catalog; Owner: postgres
--

GRANT SELECT ON TABLE pg_catalog.pg_statio_all_tables TO rsyncer;


--
-- Name: TABLE pg_statio_sys_indexes; Type: ACL; Schema: pg_catalog; Owner: postgres
--

GRANT SELECT ON TABLE pg_catalog.pg_statio_sys_indexes TO rsyncer;


--
-- Name: TABLE pg_statio_sys_sequences; Type: ACL; Schema: pg_catalog; Owner: postgres
--

GRANT SELECT ON TABLE pg_catalog.pg_statio_sys_sequences TO rsyncer;


--
-- Name: TABLE pg_statio_sys_tables; Type: ACL; Schema: pg_catalog; Owner: postgres
--

GRANT SELECT ON TABLE pg_catalog.pg_statio_sys_tables TO rsyncer;


--
-- Name: TABLE pg_statio_user_indexes; Type: ACL; Schema: pg_catalog; Owner: postgres
--

GRANT SELECT ON TABLE pg_catalog.pg_statio_user_indexes TO rsyncer;


--
-- Name: TABLE pg_statio_user_sequences; Type: ACL; Schema: pg_catalog; Owner: postgres
--

GRANT SELECT ON TABLE pg_catalog.pg_statio_user_sequences TO rsyncer;


--
-- Name: TABLE pg_statio_user_tables; Type: ACL; Schema: pg_catalog; Owner: postgres
--

GRANT SELECT ON TABLE pg_catalog.pg_statio_user_tables TO rsyncer;


--
-- Name: TABLE pg_statistic; Type: ACL; Schema: pg_catalog; Owner: postgres
--

GRANT SELECT ON TABLE pg_catalog.pg_statistic TO rsyncer;


--
-- Name: TABLE pg_statistic_ext; Type: ACL; Schema: pg_catalog; Owner: postgres
--

GRANT SELECT ON TABLE pg_catalog.pg_statistic_ext TO rsyncer;


--
-- Name: TABLE pg_stats; Type: ACL; Schema: pg_catalog; Owner: postgres
--

GRANT SELECT ON TABLE pg_catalog.pg_stats TO rsyncer;


--
-- Name: TABLE pg_subscription; Type: ACL; Schema: pg_catalog; Owner: postgres
--

GRANT SELECT ON TABLE pg_catalog.pg_subscription TO rsyncer;


--
-- Name: TABLE pg_subscription_rel; Type: ACL; Schema: pg_catalog; Owner: postgres
--

GRANT SELECT ON TABLE pg_catalog.pg_subscription_rel TO rsyncer;


--
-- Name: TABLE pg_tables; Type: ACL; Schema: pg_catalog; Owner: postgres
--

GRANT SELECT ON TABLE pg_catalog.pg_tables TO rsyncer;


--
-- Name: TABLE pg_tablespace; Type: ACL; Schema: pg_catalog; Owner: postgres
--

GRANT SELECT ON TABLE pg_catalog.pg_tablespace TO rsyncer;


--
-- Name: TABLE pg_timezone_abbrevs; Type: ACL; Schema: pg_catalog; Owner: postgres
--

GRANT SELECT ON TABLE pg_catalog.pg_timezone_abbrevs TO rsyncer;


--
-- Name: TABLE pg_timezone_names; Type: ACL; Schema: pg_catalog; Owner: postgres
--

GRANT SELECT ON TABLE pg_catalog.pg_timezone_names TO rsyncer;


--
-- Name: TABLE pg_transform; Type: ACL; Schema: pg_catalog; Owner: postgres
--

GRANT SELECT ON TABLE pg_catalog.pg_transform TO rsyncer;


--
-- Name: TABLE pg_trigger; Type: ACL; Schema: pg_catalog; Owner: postgres
--

GRANT SELECT ON TABLE pg_catalog.pg_trigger TO rsyncer;


--
-- Name: TABLE pg_ts_config; Type: ACL; Schema: pg_catalog; Owner: postgres
--

GRANT SELECT ON TABLE pg_catalog.pg_ts_config TO rsyncer;


--
-- Name: TABLE pg_ts_config_map; Type: ACL; Schema: pg_catalog; Owner: postgres
--

GRANT SELECT ON TABLE pg_catalog.pg_ts_config_map TO rsyncer;


--
-- Name: TABLE pg_ts_dict; Type: ACL; Schema: pg_catalog; Owner: postgres
--

GRANT SELECT ON TABLE pg_catalog.pg_ts_dict TO rsyncer;


--
-- Name: TABLE pg_ts_parser; Type: ACL; Schema: pg_catalog; Owner: postgres
--

GRANT SELECT ON TABLE pg_catalog.pg_ts_parser TO rsyncer;


--
-- Name: TABLE pg_ts_template; Type: ACL; Schema: pg_catalog; Owner: postgres
--

GRANT SELECT ON TABLE pg_catalog.pg_ts_template TO rsyncer;


--
-- Name: TABLE pg_type; Type: ACL; Schema: pg_catalog; Owner: postgres
--

GRANT SELECT ON TABLE pg_catalog.pg_type TO rsyncer;


--
-- Name: TABLE pg_user; Type: ACL; Schema: pg_catalog; Owner: postgres
--

GRANT SELECT ON TABLE pg_catalog.pg_user TO rsyncer;


--
-- Name: TABLE pg_user_mapping; Type: ACL; Schema: pg_catalog; Owner: postgres
--

GRANT SELECT ON TABLE pg_catalog.pg_user_mapping TO rsyncer;


--
-- Name: TABLE pg_user_mappings; Type: ACL; Schema: pg_catalog; Owner: postgres
--

GRANT SELECT ON TABLE pg_catalog.pg_user_mappings TO rsyncer;


--
-- Name: TABLE pg_views; Type: ACL; Schema: pg_catalog; Owner: postgres
--

GRANT SELECT ON TABLE pg_catalog.pg_views TO rsyncer;


--
-- Name: TABLE announcements; Type: ACL; Schema: public; Owner: dan
--

GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE public.announcements TO www;
GRANT SELECT ON TABLE public.announcements TO commits;
GRANT SELECT ON TABLE public.announcements TO rsyncer;


--
-- Name: SEQUENCE announcements_id_seq; Type: ACL; Schema: public; Owner: dan
--

REVOKE ALL ON SEQUENCE public.announcements_id_seq FROM dan;
GRANT SELECT,UPDATE ON SEQUENCE public.announcements_id_seq TO dan;
GRANT UPDATE ON SEQUENCE public.announcements_id_seq TO www;
GRANT SELECT ON SEQUENCE public.announcements_id_seq TO rsyncer;


--
-- Name: TABLE cache_clearing_dates; Type: ACL; Schema: public; Owner: dan
--

GRANT SELECT,INSERT ON TABLE public.cache_clearing_dates TO www;
GRANT SELECT,INSERT,UPDATE ON TABLE public.cache_clearing_dates TO commits;
GRANT SELECT,DELETE ON TABLE public.cache_clearing_dates TO listening;
GRANT SELECT ON TABLE public.cache_clearing_dates TO rsyncer;
GRANT SELECT ON TABLE public.cache_clearing_dates TO reporting;


--
-- Name: SEQUENCE cache_clearing_dates_id_seq; Type: ACL; Schema: public; Owner: dan
--

GRANT SELECT,UPDATE ON SEQUENCE public.cache_clearing_dates_id_seq TO www;
GRANT SELECT,UPDATE ON SEQUENCE public.cache_clearing_dates_id_seq TO commits;
GRANT SELECT ON SEQUENCE public.cache_clearing_dates_id_seq TO rsyncer;


--
-- Name: TABLE cache_clearing_ports; Type: ACL; Schema: public; Owner: dan
--

GRANT SELECT,INSERT,UPDATE ON TABLE public.cache_clearing_ports TO commits;
GRANT SELECT,INSERT ON TABLE public.cache_clearing_ports TO www;
GRANT SELECT,DELETE ON TABLE public.cache_clearing_ports TO listening;
GRANT SELECT ON TABLE public.cache_clearing_ports TO rsyncer;
GRANT SELECT ON TABLE public.cache_clearing_ports TO reporting;


--
-- Name: SEQUENCE cache_clearing_ports_id_seq; Type: ACL; Schema: public; Owner: dan
--

REVOKE ALL ON SEQUENCE public.cache_clearing_ports_id_seq FROM dan;
GRANT SELECT,UPDATE ON SEQUENCE public.cache_clearing_ports_id_seq TO dan;
GRANT SELECT,UPDATE ON SEQUENCE public.cache_clearing_ports_id_seq TO commits;
GRANT SELECT,UPDATE ON SEQUENCE public.cache_clearing_ports_id_seq TO www;
GRANT SELECT ON SEQUENCE public.cache_clearing_ports_id_seq TO rsyncer;


--
-- Name: TABLE categories; Type: ACL; Schema: public; Owner: dan
--

GRANT SELECT,UPDATE ON TABLE public.categories TO www;
GRANT SELECT,INSERT,UPDATE ON TABLE public.categories TO commits;
GRANT SELECT ON TABLE public.categories TO rsyncer;


--
-- Name: SEQUENCE categories_id_seq; Type: ACL; Schema: public; Owner: dan
--

REVOKE ALL ON SEQUENCE public.categories_id_seq FROM dan;
GRANT SELECT,UPDATE ON SEQUENCE public.categories_id_seq TO dan;
GRANT SELECT,UPDATE ON SEQUENCE public.categories_id_seq TO commits;
GRANT SELECT ON SEQUENCE public.categories_id_seq TO rsyncer;


--
-- Name: TABLE category_stats; Type: ACL; Schema: public; Owner: dan
--

GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE public.category_stats TO www;
GRANT SELECT ON TABLE public.category_stats TO rsyncer;


--
-- Name: TABLE commit_log; Type: ACL; Schema: public; Owner: dan
--

GRANT SELECT ON TABLE public.commit_log TO www;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE public.commit_log TO commits;
GRANT SELECT ON TABLE public.commit_log TO reading;
GRANT SELECT ON TABLE public.commit_log TO rsyncer;


--
-- Name: TABLE commit_log_branch_delete_me; Type: ACL; Schema: public; Owner: dan
--

GRANT SELECT ON TABLE public.commit_log_branch_delete_me TO rsyncer;


--
-- Name: TABLE commit_log_branches; Type: ACL; Schema: public; Owner: dan
--

GRANT SELECT ON TABLE public.commit_log_branches TO www;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE public.commit_log_branches TO commits;
GRANT SELECT ON TABLE public.commit_log_branches TO reading;
GRANT SELECT ON TABLE public.commit_log_branches TO rsyncer;


--
-- Name: TABLE commit_log_elements; Type: ACL; Schema: public; Owner: dan
--

GRANT SELECT ON TABLE public.commit_log_elements TO www;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE public.commit_log_elements TO commits;
GRANT SELECT ON TABLE public.commit_log_elements TO rsyncer;


--
-- Name: SEQUENCE commit_log_elements_id_seq; Type: ACL; Schema: public; Owner: dan
--

REVOKE ALL ON SEQUENCE public.commit_log_elements_id_seq FROM dan;
GRANT SELECT,UPDATE ON SEQUENCE public.commit_log_elements_id_seq TO dan;
GRANT SELECT,UPDATE ON SEQUENCE public.commit_log_elements_id_seq TO commits;
GRANT SELECT ON SEQUENCE public.commit_log_elements_id_seq TO rsyncer;


--
-- Name: SEQUENCE commit_log_id_seq; Type: ACL; Schema: public; Owner: dan
--

REVOKE ALL ON SEQUENCE public.commit_log_id_seq FROM dan;
GRANT SELECT,UPDATE ON SEQUENCE public.commit_log_id_seq TO dan;
GRANT SELECT,UPDATE ON SEQUENCE public.commit_log_id_seq TO commits;
GRANT SELECT ON SEQUENCE public.commit_log_id_seq TO rsyncer;


--
-- Name: TABLE commit_log_port_elements; Type: ACL; Schema: public; Owner: dan
--

GRANT SELECT ON TABLE public.commit_log_port_elements TO www;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE public.commit_log_port_elements TO commits;
GRANT SELECT ON TABLE public.commit_log_port_elements TO rsyncer;


--
-- Name: TABLE commit_log_ports; Type: ACL; Schema: public; Owner: dan
--

GRANT SELECT ON TABLE public.commit_log_ports TO www;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE public.commit_log_ports TO commits;
GRANT SELECT ON TABLE public.commit_log_ports TO reading;
GRANT SELECT ON TABLE public.commit_log_ports TO rsyncer;


--
-- Name: TABLE commit_log_ports_elements; Type: ACL; Schema: public; Owner: dan
--

GRANT SELECT ON TABLE public.commit_log_ports_elements TO www;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE public.commit_log_ports_elements TO commits;
GRANT SELECT ON TABLE public.commit_log_ports_elements TO rsyncer;


--
-- Name: TABLE commit_log_ports_ignore; Type: ACL; Schema: public; Owner: dan
--

GRANT SELECT ON TABLE public.commit_log_ports_ignore TO www;
GRANT SELECT,INSERT ON TABLE public.commit_log_ports_ignore TO commits;
GRANT SELECT ON TABLE public.commit_log_ports_ignore TO rsyncer;


--
-- Name: TABLE commit_log_ports_vuxml; Type: ACL; Schema: public; Owner: dan
--

GRANT SELECT ON TABLE public.commit_log_ports_vuxml TO www;
GRANT SELECT,INSERT,DELETE ON TABLE public.commit_log_ports_vuxml TO commits;
GRANT SELECT ON TABLE public.commit_log_ports_vuxml TO rsyncer;


--
-- Name: SEQUENCE commit_log_ports_vuxml_id_seq; Type: ACL; Schema: public; Owner: dan
--

REVOKE ALL ON SEQUENCE public.commit_log_ports_vuxml_id_seq FROM dan;
GRANT SELECT,UPDATE ON SEQUENCE public.commit_log_ports_vuxml_id_seq TO dan;
GRANT UPDATE ON SEQUENCE public.commit_log_ports_vuxml_id_seq TO commits;
GRANT SELECT ON SEQUENCE public.commit_log_ports_vuxml_id_seq TO rsyncer;


--
-- Name: TABLE commits_flagged; Type: ACL; Schema: public; Owner: dan
--

GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE public.commits_flagged TO www;
GRANT SELECT ON TABLE public.commits_flagged TO rsyncer;


--
-- Name: TABLE commits_latest; Type: ACL; Schema: public; Owner: dan
--

GRANT SELECT ON TABLE public.commits_latest TO www;
GRANT SELECT,INSERT,DELETE ON TABLE public.commits_latest TO commits;
GRANT SELECT ON TABLE public.commits_latest TO rsyncer;


--
-- Name: TABLE commits_latest_ports; Type: ACL; Schema: public; Owner: dan
--

GRANT SELECT ON TABLE public.commits_latest_ports TO rsyncer;


--
-- Name: TABLE commits_recent; Type: ACL; Schema: public; Owner: dan
--

GRANT SELECT ON TABLE public.commits_recent TO commits;
GRANT SELECT ON TABLE public.commits_recent TO rsyncer;


--
-- Name: TABLE commits_recent_ports; Type: ACL; Schema: public; Owner: dan
--

GRANT SELECT ON TABLE public.commits_recent_ports TO commits;
GRANT SELECT ON TABLE public.commits_recent_ports TO rsyncer;


--
-- Name: TABLE committer_notify; Type: ACL; Schema: public; Owner: dan
--

GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE public.committer_notify TO www;
GRANT SELECT ON TABLE public.committer_notify TO commits;
GRANT SELECT ON TABLE public.committer_notify TO rsyncer;


--
-- Name: TABLE daily_refreshes; Type: ACL; Schema: public; Owner: dan
--

GRANT SELECT,INSERT,DELETE ON TABLE public.daily_refreshes TO commits;
GRANT SELECT ON TABLE public.daily_refreshes TO rsyncer;


--
-- Name: TABLE daily_stats; Type: ACL; Schema: public; Owner: dan
--

GRANT SELECT ON TABLE public.daily_stats TO www;
GRANT SELECT ON TABLE public.daily_stats TO commits;
GRANT SELECT ON TABLE public.daily_stats TO rsyncer;


--
-- Name: TABLE daily_stats_data; Type: ACL; Schema: public; Owner: dan
--

GRANT SELECT ON TABLE public.daily_stats_data TO www;
GRANT INSERT ON TABLE public.daily_stats_data TO commits;
GRANT SELECT ON TABLE public.daily_stats_data TO rsyncer;


--
-- Name: SEQUENCE daily_stats_data_seq; Type: ACL; Schema: public; Owner: dan
--

REVOKE ALL ON SEQUENCE public.daily_stats_data_seq FROM dan;
GRANT SELECT,UPDATE ON SEQUENCE public.daily_stats_data_seq TO dan;
GRANT UPDATE ON SEQUENCE public.daily_stats_data_seq TO commits;
GRANT SELECT ON SEQUENCE public.daily_stats_data_seq TO rsyncer;


--
-- Name: SEQUENCE daily_stats_seq; Type: ACL; Schema: public; Owner: dan
--

GRANT SELECT ON SEQUENCE public.daily_stats_seq TO rsyncer;


--
-- Name: TABLE design_results; Type: ACL; Schema: public; Owner: dan
--

GRANT SELECT,INSERT ON TABLE public.design_results TO www;
GRANT SELECT ON TABLE public.design_results TO rsyncer;


--
-- Name: SEQUENCE design_results_id_seq; Type: ACL; Schema: public; Owner: dan
--

REVOKE ALL ON SEQUENCE public.design_results_id_seq FROM dan;
GRANT SELECT,UPDATE ON SEQUENCE public.design_results_id_seq TO dan;
GRANT SELECT,UPDATE ON SEQUENCE public.design_results_id_seq TO www;
GRANT SELECT ON SEQUENCE public.design_results_id_seq TO rsyncer;


--
-- Name: TABLE element; Type: ACL; Schema: public; Owner: dan
--

GRANT SELECT ON TABLE public.element TO www;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE public.element TO commits;
GRANT SELECT ON TABLE public.element TO reading;
GRANT SELECT ON TABLE public.element TO rsyncer;


--
-- Name: SEQUENCE element_id_seq; Type: ACL; Schema: public; Owner: dan
--

REVOKE ALL ON SEQUENCE public.element_id_seq FROM dan;
GRANT SELECT,UPDATE ON SEQUENCE public.element_id_seq TO dan;
GRANT SELECT,UPDATE ON SEQUENCE public.element_id_seq TO commits;
GRANT SELECT ON SEQUENCE public.element_id_seq TO rsyncer;


--
-- Name: TABLE element_pathname; Type: ACL; Schema: public; Owner: dan
--

GRANT SELECT ON TABLE public.element_pathname TO www;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE public.element_pathname TO commits;
GRANT SELECT ON TABLE public.element_pathname TO rsyncer;
GRANT SELECT ON TABLE public.element_pathname TO reading;


--
-- Name: TABLE element_revision; Type: ACL; Schema: public; Owner: dan
--

GRANT SELECT ON TABLE public.element_revision TO www;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE public.element_revision TO commits;
GRANT SELECT ON TABLE public.element_revision TO rsyncer;


--
-- Name: SEQUENCE flavors_id_seq; Type: ACL; Schema: public; Owner: dan
--

GRANT SELECT,UPDATE ON SEQUENCE public.flavors_id_seq TO commits;
GRANT SELECT ON SEQUENCE public.flavors_id_seq TO rsyncer;


--
-- Name: TABLE flavors; Type: ACL; Schema: public; Owner: dan
--

GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE public.flavors TO commits;
GRANT SELECT ON TABLE public.flavors TO www;
GRANT SELECT ON TABLE public.flavors TO rsyncer;


--
-- Name: SEQUENCE generate_plist_id_seq; Type: ACL; Schema: public; Owner: dan
--

GRANT SELECT,UPDATE ON SEQUENCE public.generate_plist_id_seq TO commits;
GRANT SELECT ON SEQUENCE public.generate_plist_id_seq TO rsyncer;


--
-- Name: TABLE generate_plist; Type: ACL; Schema: public; Owner: dan
--

GRANT SELECT ON TABLE public.generate_plist TO www;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE public.generate_plist TO commits;
GRANT SELECT ON TABLE public.generate_plist TO rsyncer;


--
-- Name: TABLE graphs; Type: ACL; Schema: public; Owner: dan
--

GRANT SELECT ON TABLE public.graphs TO www;
GRANT SELECT ON TABLE public.graphs TO rsyncer;


--
-- Name: SEQUENCE graphs_id_seq; Type: ACL; Schema: public; Owner: dan
--

GRANT SELECT ON SEQUENCE public.graphs_id_seq TO rsyncer;


--
-- Name: TABLE latest_commits; Type: ACL; Schema: public; Owner: dan
--

GRANT SELECT ON TABLE public.latest_commits TO www;
GRANT SELECT,INSERT,DELETE ON TABLE public.latest_commits TO commits;
GRANT SELECT ON TABLE public.latest_commits TO rsyncer;


--
-- Name: TABLE latest_commits_ports; Type: ACL; Schema: public; Owner: dan
--

GRANT SELECT ON TABLE public.latest_commits_ports TO www;
GRANT SELECT,INSERT,DELETE ON TABLE public.latest_commits_ports TO commits;
GRANT SELECT ON TABLE public.latest_commits_ports TO rsyncer;


--
-- Name: TABLE listen_for; Type: ACL; Schema: public; Owner: dan
--

GRANT SELECT ON TABLE public.listen_for TO listening;
GRANT SELECT ON TABLE public.listen_for TO rsyncer;


--
-- Name: SEQUENCE listen_for_id_seq; Type: ACL; Schema: public; Owner: dan
--

GRANT SELECT ON SEQUENCE public.listen_for_id_seq TO rsyncer;


--
-- Name: TABLE maxcommitid; Type: ACL; Schema: public; Owner: dan
--

GRANT SELECT ON TABLE public.maxcommitid TO rsyncer;


--
-- Name: SEQUENCE package_flavors_id_seq; Type: ACL; Schema: public; Owner: dan
--

GRANT SELECT,UPDATE ON SEQUENCE public.package_flavors_id_seq TO commits;
GRANT SELECT ON SEQUENCE public.package_flavors_id_seq TO rsyncer;


--
-- Name: TABLE package_flavors; Type: ACL; Schema: public; Owner: dan
--

GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE public.package_flavors TO commits;
GRANT SELECT ON TABLE public.package_flavors TO www;
GRANT SELECT ON TABLE public.package_flavors TO rsyncer;


--
-- Name: TABLE page_load_detail; Type: ACL; Schema: public; Owner: dan
--

GRANT INSERT ON TABLE public.page_load_detail TO www;
GRANT SELECT,DELETE ON TABLE public.page_load_detail TO commits;
GRANT SELECT ON TABLE public.page_load_detail TO rsyncer;


--
-- Name: SEQUENCE page_load_detail_id_seq; Type: ACL; Schema: public; Owner: dan
--

REVOKE ALL ON SEQUENCE public.page_load_detail_id_seq FROM dan;
GRANT SELECT,UPDATE ON SEQUENCE public.page_load_detail_id_seq TO dan;
GRANT UPDATE ON SEQUENCE public.page_load_detail_id_seq TO www;
GRANT SELECT ON SEQUENCE public.page_load_detail_id_seq TO rsyncer;


--
-- Name: TABLE page_load_summary; Type: ACL; Schema: public; Owner: dan
--

GRANT INSERT ON TABLE public.page_load_summary TO commits;
GRANT SELECT ON TABLE public.page_load_summary TO rsyncer;


--
-- Name: SEQUENCE page_load_summary_id_seq; Type: ACL; Schema: public; Owner: dan
--

REVOKE ALL ON SEQUENCE public.page_load_summary_id_seq FROM dan;
GRANT SELECT,UPDATE ON SEQUENCE public.page_load_summary_id_seq TO dan;
GRANT UPDATE ON SEQUENCE public.page_load_summary_id_seq TO commits;
GRANT SELECT ON SEQUENCE public.page_load_summary_id_seq TO rsyncer;


--
-- Name: TABLE port_dependencies; Type: ACL; Schema: public; Owner: dan
--

GRANT SELECT ON TABLE public.port_dependencies TO www;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE public.port_dependencies TO commits;
GRANT SELECT ON TABLE public.port_dependencies TO rsyncer;


--
-- Name: TABLE portcount; Type: ACL; Schema: public; Owner: dan
--

GRANT SELECT ON TABLE public.portcount TO rsyncer;


--
-- Name: TABLE ports; Type: ACL; Schema: public; Owner: dan
--

GRANT SELECT ON TABLE public.ports TO www;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE public.ports TO commits;
GRANT SELECT ON TABLE public.ports TO reading;
GRANT SELECT ON TABLE public.ports TO rsyncer;


--
-- Name: TABLE ports_active; Type: ACL; Schema: public; Owner: dan
--

GRANT SELECT ON TABLE public.ports_active TO www;
GRANT SELECT ON TABLE public.ports_active TO commits;
GRANT SELECT ON TABLE public.ports_active TO reading;
GRANT SELECT ON TABLE public.ports_active TO rsyncer;


--
-- Name: TABLE ports_all; Type: ACL; Schema: public; Owner: dan
--

GRANT SELECT ON TABLE public.ports_all TO www;
GRANT SELECT ON TABLE public.ports_all TO commits;
GRANT SELECT ON TABLE public.ports_all TO rsyncer;


--
-- Name: TABLE ports_categories; Type: ACL; Schema: public; Owner: dan
--

GRANT SELECT ON TABLE public.ports_categories TO www;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE public.ports_categories TO commits;
GRANT SELECT ON TABLE public.ports_categories TO reading;
GRANT SELECT ON TABLE public.ports_categories TO rsyncer;


--
-- Name: TABLE ports_check; Type: ACL; Schema: public; Owner: dan
--

GRANT ALL ON TABLE public.ports_check TO commits;
GRANT SELECT ON TABLE public.ports_check TO rsyncer;


--
-- Name: SEQUENCE ports_check_id_seq; Type: ACL; Schema: public; Owner: dan
--

REVOKE ALL ON SEQUENCE public.ports_check_id_seq FROM dan;
GRANT SELECT,UPDATE ON SEQUENCE public.ports_check_id_seq TO dan;
GRANT UPDATE ON SEQUENCE public.ports_check_id_seq TO commits;
GRANT SELECT ON SEQUENCE public.ports_check_id_seq TO rsyncer;


--
-- Name: TABLE ports_conflicts; Type: ACL; Schema: public; Owner: dan
--

GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE public.ports_conflicts TO commits;
GRANT SELECT ON TABLE public.ports_conflicts TO rsyncer;
GRANT SELECT ON TABLE public.ports_conflicts TO www;


--
-- Name: SEQUENCE ports_conflicts_id_seq; Type: ACL; Schema: public; Owner: dan
--

GRANT SELECT,UPDATE ON SEQUENCE public.ports_conflicts_id_seq TO commits;
GRANT SELECT ON SEQUENCE public.ports_conflicts_id_seq TO rsyncer;


--
-- Name: SEQUENCE ports_id_seq; Type: ACL; Schema: public; Owner: dan
--

REVOKE ALL ON SEQUENCE public.ports_id_seq FROM dan;
GRANT SELECT,UPDATE ON SEQUENCE public.ports_id_seq TO dan;
GRANT SELECT,UPDATE ON SEQUENCE public.ports_id_seq TO commits;
GRANT SELECT ON SEQUENCE public.ports_id_seq TO rsyncer;


--
-- Name: TABLE ports_moved; Type: ACL; Schema: public; Owner: dan
--

GRANT SELECT ON TABLE public.ports_moved TO www;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE public.ports_moved TO commits;
GRANT SELECT ON TABLE public.ports_moved TO rsyncer;


--
-- Name: SEQUENCE ports_moved_id_seq; Type: ACL; Schema: public; Owner: dan
--

REVOKE ALL ON SEQUENCE public.ports_moved_id_seq FROM dan;
GRANT SELECT,UPDATE ON SEQUENCE public.ports_moved_id_seq TO dan;
GRANT UPDATE ON SEQUENCE public.ports_moved_id_seq TO commits;
GRANT SELECT ON SEQUENCE public.ports_moved_id_seq TO rsyncer;


--
-- Name: TABLE ports_updating; Type: ACL; Schema: public; Owner: dan
--

GRANT SELECT ON TABLE public.ports_updating TO www;
GRANT INSERT,DELETE,UPDATE ON TABLE public.ports_updating TO commits;
GRANT SELECT ON TABLE public.ports_updating TO rsyncer;


--
-- Name: SEQUENCE ports_updating_id_seq; Type: ACL; Schema: public; Owner: dan
--

REVOKE ALL ON SEQUENCE public.ports_updating_id_seq FROM dan;
GRANT SELECT,UPDATE ON SEQUENCE public.ports_updating_id_seq TO dan;
GRANT UPDATE ON SEQUENCE public.ports_updating_id_seq TO commits;
GRANT SELECT ON SEQUENCE public.ports_updating_id_seq TO rsyncer;


--
-- Name: TABLE ports_updating_ports_xref; Type: ACL; Schema: public; Owner: dan
--

GRANT SELECT ON TABLE public.ports_updating_ports_xref TO www;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE public.ports_updating_ports_xref TO commits;
GRANT SELECT ON TABLE public.ports_updating_ports_xref TO rsyncer;


--
-- Name: TABLE ports_vulnerable; Type: ACL; Schema: public; Owner: dan
--

GRANT SELECT ON TABLE public.ports_vulnerable TO www;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE public.ports_vulnerable TO commits;
GRANT SELECT ON TABLE public.ports_vulnerable TO reading;
GRANT SELECT ON TABLE public.ports_vulnerable TO rsyncer;


--
-- Name: TABLE repo; Type: ACL; Schema: public; Owner: dan
--

GRANT SELECT ON TABLE public.repo TO www;
GRANT SELECT ON TABLE public.repo TO commits;
GRANT SELECT ON TABLE public.repo TO rsyncer;


--
-- Name: SEQUENCE repo_id_seq; Type: ACL; Schema: public; Owner: dan
--

GRANT SELECT ON SEQUENCE public.repo_id_seq TO rsyncer;


--
-- Name: TABLE report_frequency; Type: ACL; Schema: public; Owner: dan
--

GRANT SELECT ON TABLE public.report_frequency TO www;
GRANT SELECT ON TABLE public.report_frequency TO commits;
GRANT SELECT ON TABLE public.report_frequency TO rsyncer;


--
-- Name: SEQUENCE report_frequency_id_seq; Type: ACL; Schema: public; Owner: dan
--

GRANT SELECT ON SEQUENCE public.report_frequency_id_seq TO rsyncer;


--
-- Name: TABLE report_log; Type: ACL; Schema: public; Owner: dan
--

GRANT SELECT ON TABLE public.report_log TO www;
GRANT SELECT,INSERT ON TABLE public.report_log TO commits;
GRANT SELECT ON TABLE public.report_log TO rsyncer;


--
-- Name: SEQUENCE report_log_id_seq; Type: ACL; Schema: public; Owner: dan
--

REVOKE ALL ON SEQUENCE public.report_log_id_seq FROM dan;
GRANT SELECT,UPDATE ON SEQUENCE public.report_log_id_seq TO dan;
GRANT UPDATE ON SEQUENCE public.report_log_id_seq TO commits;
GRANT SELECT ON SEQUENCE public.report_log_id_seq TO rsyncer;


--
-- Name: TABLE report_log_latest; Type: ACL; Schema: public; Owner: dan
--

GRANT SELECT ON TABLE public.report_log_latest TO commits;
GRANT SELECT ON TABLE public.report_log_latest TO rsyncer;


--
-- Name: TABLE report_subscriptions; Type: ACL; Schema: public; Owner: dan
--

GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE public.report_subscriptions TO www;
GRANT SELECT ON TABLE public.report_subscriptions TO commits;
GRANT SELECT ON TABLE public.report_subscriptions TO reading;
GRANT SELECT ON TABLE public.report_subscriptions TO rsyncer;


--
-- Name: TABLE reports; Type: ACL; Schema: public; Owner: dan
--

GRANT SELECT ON TABLE public.reports TO www;
GRANT SELECT ON TABLE public.reports TO commits;
GRANT SELECT ON TABLE public.reports TO rsyncer;


--
-- Name: SEQUENCE reports_id_seq; Type: ACL; Schema: public; Owner: dan
--

GRANT SELECT ON SEQUENCE public.reports_id_seq TO rsyncer;


--
-- Name: TABLE sanity_test_failures; Type: ACL; Schema: public; Owner: dan
--

GRANT SELECT ON TABLE public.sanity_test_failures TO www;
GRANT INSERT ON TABLE public.sanity_test_failures TO commits;
GRANT SELECT ON TABLE public.sanity_test_failures TO rsyncer;


--
-- Name: SEQUENCE sanity_test_failures_id_seq; Type: ACL; Schema: public; Owner: dan
--

REVOKE ALL ON SEQUENCE public.sanity_test_failures_id_seq FROM dan;
GRANT SELECT,UPDATE ON SEQUENCE public.sanity_test_failures_id_seq TO dan;
GRANT UPDATE ON SEQUENCE public.sanity_test_failures_id_seq TO commits;
GRANT SELECT ON SEQUENCE public.sanity_test_failures_id_seq TO rsyncer;


--
-- Name: TABLE security_notice; Type: ACL; Schema: public; Owner: dan
--

GRANT SELECT,INSERT,UPDATE ON TABLE public.security_notice TO www;
GRANT SELECT ON TABLE public.security_notice TO commits;
GRANT SELECT ON TABLE public.security_notice TO rsyncer;


--
-- Name: TABLE security_notice_audit; Type: ACL; Schema: public; Owner: dan
--

GRANT SELECT,INSERT,UPDATE ON TABLE public.security_notice_audit TO www;
GRANT SELECT ON TABLE public.security_notice_audit TO commits;
GRANT SELECT ON TABLE public.security_notice_audit TO rsyncer;


--
-- Name: SEQUENCE security_notice_audit_id_seq; Type: ACL; Schema: public; Owner: dan
--

REVOKE ALL ON SEQUENCE public.security_notice_audit_id_seq FROM dan;
GRANT SELECT,UPDATE ON SEQUENCE public.security_notice_audit_id_seq TO dan;
GRANT SELECT,UPDATE ON SEQUENCE public.security_notice_audit_id_seq TO www;
GRANT SELECT ON SEQUENCE public.security_notice_audit_id_seq TO rsyncer;


--
-- Name: SEQUENCE security_notice_id_seq; Type: ACL; Schema: public; Owner: dan
--

REVOKE ALL ON SEQUENCE public.security_notice_id_seq FROM dan;
GRANT SELECT,UPDATE ON SEQUENCE public.security_notice_id_seq TO dan;
GRANT UPDATE ON SEQUENCE public.security_notice_id_seq TO www;
GRANT SELECT ON SEQUENCE public.security_notice_id_seq TO rsyncer;


--
-- Name: TABLE security_notice_status; Type: ACL; Schema: public; Owner: dan
--

GRANT SELECT ON TABLE public.security_notice_status TO rsyncer;


--
-- Name: TABLE system; Type: ACL; Schema: public; Owner: dan
--

GRANT SELECT ON TABLE public.system TO www;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE public.system TO commits;
GRANT SELECT ON TABLE public.system TO rsyncer;


--
-- Name: TABLE system_branch; Type: ACL; Schema: public; Owner: dan
--

GRANT SELECT ON TABLE public.system_branch TO www;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE public.system_branch TO commits;
GRANT SELECT ON TABLE public.system_branch TO reading;
GRANT SELECT ON TABLE public.system_branch TO rsyncer;


--
-- Name: TABLE system_branch_element_revision; Type: ACL; Schema: public; Owner: dan
--

GRANT SELECT ON TABLE public.system_branch_element_revision TO www;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE public.system_branch_element_revision TO commits;
GRANT SELECT ON TABLE public.system_branch_element_revision TO rsyncer;


--
-- Name: SEQUENCE system_branch_id_seq; Type: ACL; Schema: public; Owner: dan
--

REVOKE ALL ON SEQUENCE public.system_branch_id_seq FROM dan;
GRANT SELECT,UPDATE ON SEQUENCE public.system_branch_id_seq TO dan;
GRANT SELECT,UPDATE ON SEQUENCE public.system_branch_id_seq TO commits;
GRANT SELECT ON SEQUENCE public.system_branch_id_seq TO rsyncer;


--
-- Name: SEQUENCE system_id_seq; Type: ACL; Schema: public; Owner: dan
--

GRANT SELECT ON SEQUENCE public.system_id_seq TO rsyncer;


--
-- Name: TABLE tasks; Type: ACL; Schema: public; Owner: dan
--

GRANT SELECT ON TABLE public.tasks TO www;
GRANT SELECT ON TABLE public.tasks TO rsyncer;


--
-- Name: SEQUENCE tasks_id_seq; Type: ACL; Schema: public; Owner: dan
--

GRANT SELECT ON SEQUENCE public.tasks_id_seq TO rsyncer;


--
-- Name: TABLE user_confirmations; Type: ACL; Schema: public; Owner: dan
--

GRANT SELECT,INSERT,DELETE ON TABLE public.user_confirmations TO www;
GRANT SELECT ON TABLE public.user_confirmations TO rsyncer;


--
-- Name: TABLE user_password_reset; Type: ACL; Schema: public; Owner: dan
--

GRANT SELECT,INSERT,DELETE ON TABLE public.user_password_reset TO www;
GRANT SELECT,DELETE ON TABLE public.user_password_reset TO commits;
GRANT SELECT ON TABLE public.user_password_reset TO rsyncer;


--
-- Name: TABLE user_tasks; Type: ACL; Schema: public; Owner: dan
--

GRANT SELECT ON TABLE public.user_tasks TO www;
GRANT SELECT ON TABLE public.user_tasks TO rsyncer;


--
-- Name: TABLE users; Type: ACL; Schema: public; Owner: dan
--

GRANT SELECT,INSERT,UPDATE ON TABLE public.users TO www;
GRANT SELECT ON TABLE public.users TO commits;
GRANT SELECT ON TABLE public.users TO reading;
GRANT SELECT ON TABLE public.users TO rsyncer;


--
-- Name: SEQUENCE users_id_seq; Type: ACL; Schema: public; Owner: dan
--

REVOKE ALL ON SEQUENCE public.users_id_seq FROM dan;
GRANT SELECT,UPDATE ON SEQUENCE public.users_id_seq TO dan;
GRANT SELECT,UPDATE ON SEQUENCE public.users_id_seq TO www;
GRANT SELECT ON SEQUENCE public.users_id_seq TO rsyncer;


--
-- Name: TABLE vuxml; Type: ACL; Schema: public; Owner: dan
--

GRANT SELECT ON TABLE public.vuxml TO www;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE public.vuxml TO commits;
GRANT SELECT ON TABLE public.vuxml TO rsyncer;


--
-- Name: TABLE vuxml_affected; Type: ACL; Schema: public; Owner: dan
--

GRANT SELECT ON TABLE public.vuxml_affected TO www;
GRANT SELECT,INSERT ON TABLE public.vuxml_affected TO commits;
GRANT SELECT ON TABLE public.vuxml_affected TO rsyncer;


--
-- Name: SEQUENCE vuxml_affected_id_seq; Type: ACL; Schema: public; Owner: dan
--

REVOKE ALL ON SEQUENCE public.vuxml_affected_id_seq FROM dan;
GRANT SELECT,UPDATE ON SEQUENCE public.vuxml_affected_id_seq TO dan;
GRANT UPDATE ON SEQUENCE public.vuxml_affected_id_seq TO commits;
GRANT SELECT ON SEQUENCE public.vuxml_affected_id_seq TO rsyncer;


--
-- Name: SEQUENCE vuxml_id_seq; Type: ACL; Schema: public; Owner: dan
--

REVOKE ALL ON SEQUENCE public.vuxml_id_seq FROM dan;
GRANT SELECT,UPDATE ON SEQUENCE public.vuxml_id_seq TO dan;
GRANT UPDATE ON SEQUENCE public.vuxml_id_seq TO commits;
GRANT SELECT ON SEQUENCE public.vuxml_id_seq TO rsyncer;


--
-- Name: TABLE vuxml_names; Type: ACL; Schema: public; Owner: dan
--

GRANT SELECT ON TABLE public.vuxml_names TO www;
GRANT SELECT,INSERT ON TABLE public.vuxml_names TO commits;
GRANT SELECT ON TABLE public.vuxml_names TO rsyncer;


--
-- Name: SEQUENCE vuxml_names_id_seq; Type: ACL; Schema: public; Owner: dan
--

REVOKE ALL ON SEQUENCE public.vuxml_names_id_seq FROM dan;
GRANT SELECT,UPDATE ON SEQUENCE public.vuxml_names_id_seq TO dan;
GRANT UPDATE ON SEQUENCE public.vuxml_names_id_seq TO commits;
GRANT SELECT ON SEQUENCE public.vuxml_names_id_seq TO rsyncer;


--
-- Name: TABLE vuxml_ranges; Type: ACL; Schema: public; Owner: dan
--

GRANT SELECT,INSERT ON TABLE public.vuxml_ranges TO commits;
GRANT SELECT ON TABLE public.vuxml_ranges TO www;
GRANT SELECT ON TABLE public.vuxml_ranges TO rsyncer;


--
-- Name: SEQUENCE vuxml_ranges_id_seq; Type: ACL; Schema: public; Owner: dan
--

REVOKE ALL ON SEQUENCE public.vuxml_ranges_id_seq FROM dan;
GRANT SELECT,UPDATE ON SEQUENCE public.vuxml_ranges_id_seq TO dan;
GRANT UPDATE ON SEQUENCE public.vuxml_ranges_id_seq TO commits;
GRANT SELECT ON SEQUENCE public.vuxml_ranges_id_seq TO rsyncer;


--
-- Name: TABLE vuxml_references; Type: ACL; Schema: public; Owner: dan
--

GRANT INSERT ON TABLE public.vuxml_references TO commits;
GRANT SELECT ON TABLE public.vuxml_references TO www;
GRANT SELECT ON TABLE public.vuxml_references TO rsyncer;


--
-- Name: SEQUENCE vuxml_references_id_seq; Type: ACL; Schema: public; Owner: dan
--

REVOKE ALL ON SEQUENCE public.vuxml_references_id_seq FROM dan;
GRANT SELECT,UPDATE ON SEQUENCE public.vuxml_references_id_seq TO dan;
GRANT UPDATE ON SEQUENCE public.vuxml_references_id_seq TO commits;
GRANT SELECT ON SEQUENCE public.vuxml_references_id_seq TO rsyncer;


--
-- Name: TABLE watch_list; Type: ACL; Schema: public; Owner: dan
--

GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE public.watch_list TO www;
GRANT SELECT ON TABLE public.watch_list TO commits;
GRANT SELECT ON TABLE public.watch_list TO rsyncer;


--
-- Name: TABLE watch_list_element; Type: ACL; Schema: public; Owner: dan
--

GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE public.watch_list_element TO www;
GRANT SELECT ON TABLE public.watch_list_element TO commits;
GRANT SELECT ON TABLE public.watch_list_element TO rsyncer;


--
-- Name: SEQUENCE watch_list_id_seq; Type: ACL; Schema: public; Owner: dan
--

REVOKE ALL ON SEQUENCE public.watch_list_id_seq FROM dan;
GRANT SELECT,UPDATE ON SEQUENCE public.watch_list_id_seq TO dan;
GRANT SELECT,UPDATE ON SEQUENCE public.watch_list_id_seq TO www;
GRANT SELECT ON SEQUENCE public.watch_list_id_seq TO rsyncer;


--
-- Name: TABLE watch_list_staging; Type: ACL; Schema: public; Owner: dan
--

GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE public.watch_list_staging TO www;
GRANT SELECT ON TABLE public.watch_list_staging TO rsyncer;


--
-- Name: SEQUENCE watch_list_staging_id_seq; Type: ACL; Schema: public; Owner: dan
--

REVOKE ALL ON SEQUENCE public.watch_list_staging_id_seq FROM dan;
GRANT SELECT,UPDATE ON SEQUENCE public.watch_list_staging_id_seq TO dan;
GRANT SELECT,UPDATE ON SEQUENCE public.watch_list_staging_id_seq TO www;
GRANT SELECT ON SEQUENCE public.watch_list_staging_id_seq TO rsyncer;


--
-- Name: TABLE watch_list_staging_log; Type: ACL; Schema: public; Owner: dan
--

GRANT INSERT ON TABLE public.watch_list_staging_log TO www;
GRANT SELECT ON TABLE public.watch_list_staging_log TO rsyncer;


--
-- Name: SEQUENCE watch_list_staging_log_id_seq; Type: ACL; Schema: public; Owner: dan
--

REVOKE ALL ON SEQUENCE public.watch_list_staging_log_id_seq FROM dan;
GRANT SELECT,UPDATE ON SEQUENCE public.watch_list_staging_log_id_seq TO dan;
GRANT UPDATE ON SEQUENCE public.watch_list_staging_log_id_seq TO www;
GRANT SELECT ON SEQUENCE public.watch_list_staging_log_id_seq TO rsyncer;


--
-- Name: TABLE watch_notice; Type: ACL; Schema: public; Owner: dan
--

GRANT SELECT ON TABLE public.watch_notice TO www;
GRANT SELECT,INSERT,UPDATE ON TABLE public.watch_notice TO commits;
GRANT SELECT ON TABLE public.watch_notice TO rsyncer;


--
-- Name: SEQUENCE watch_notice_id_seq; Type: ACL; Schema: public; Owner: dan
--

GRANT SELECT ON SEQUENCE public.watch_notice_id_seq TO rsyncer;


--
-- Name: TABLE watch_notice_log; Type: ACL; Schema: public; Owner: dan
--

GRANT SELECT,INSERT,UPDATE ON TABLE public.watch_notice_log TO commits;
GRANT SELECT ON TABLE public.watch_notice_log TO rsyncer;


--
-- Name: SEQUENCE watch_notice_log_id_seq; Type: ACL; Schema: public; Owner: dan
--

REVOKE ALL ON SEQUENCE public.watch_notice_log_id_seq FROM dan;
GRANT SELECT,UPDATE ON SEQUENCE public.watch_notice_log_id_seq TO dan;
GRANT UPDATE ON SEQUENCE public.watch_notice_log_id_seq TO commits;
GRANT SELECT ON SEQUENCE public.watch_notice_log_id_seq TO rsyncer;


--
-- PostgreSQL database dump complete
--

