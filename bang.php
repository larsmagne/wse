<?php
/*
Plugin Name: Bang
Description: Collect usage statistics.
*/

function add_bang_js() {
  wp_enqueue_script("bang", "/wp-content/plugins/bang/bang.js");
}

add_action("wp_enqueue_scripts", "add_bang_js");

function bang_create_database_table() {
  global $wpdb;
  $table_name = $wpdb->prefix . 'bang_stats';
  $charset_collate = $wpdb->get_charset_collate();

  $sql = "CREATE TABLE $table_name (
        id int NOT NULL AUTO_INCREMENT,
        time datetime DEFAULT '0000-00-00 00:00:00' NOT NULL,
        click text,
        page text,
        referrer text,
        ip text,
        user_agent text,
        title text,
        primary key (id)
    ) $charset_collate;";

  require_once ABSPATH . 'wp-admin/includes/upgrade.php';
  dbDelta($sql);
}

function bang_delete_table() {
  global $wpdb;
  $table_name = $wpdb->prefix . 'bang_stats';
  $wpdb->query("DROP TABLE IF EXISTS $table_name");
}

register_activation_hook(__FILE__, 'bang_create_database_table');
register_deactivation_hook(__FILE__, 'bang_delete_table');
