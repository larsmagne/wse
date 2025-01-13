<?php
/*
Plugin Name: WSE
Description: Collect usage statistics.
*/

function add_wse_js() {
  wp_enqueue_script("wse", "/wp-content/plugins/wse/wse.js");
}

add_action("wp_enqueue_scripts", "add_wse_js");

// Function run when the plugin is activated.
function wse_create_database_table() {
  global $wpdb;
  $table_name = $wpdb->prefix . 'wse_stats';
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

// Function run when the plugin is deactivated.
function wse_delete_table() {
  global $wpdb;
  $table_name = $wpdb->prefix . 'wse_stats';
  $wpdb->query("DROP TABLE IF EXISTS $table_name");
}

register_activation_hook(__FILE__, 'wse_create_database_table');
register_deactivation_hook(__FILE__, 'wse_delete_table');
