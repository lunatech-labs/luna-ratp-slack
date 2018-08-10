SET statement_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SET check_function_bodies = false;
SET client_min_messages = warning;

SET search_path TO ratpbot, pg_catalog;

DROP SCHEMA IF EXISTS ratpbot CASCADE ;

CREATE SCHEMA ratpbot;

SET default_tablespace = '';
SET default_with_oids = false;
SET search_path TO ratpbot, pg_catalog;

DROP FUNCTION IF EXISTS public.last_updated();

CREATE FUNCTION public.last_updated() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
BEGIN
    NEW.last_update = CURRENT_TIMESTAMP;
    RETURN NEW;
END $$;
