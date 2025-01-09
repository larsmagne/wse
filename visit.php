<?php
require_once($_SERVER['DOCUMENT_ROOT'] . '/wp-config.php');
$wp->init();
$wp->parse_request();
$wp->query_posts();
$wp->register_globals();
$wp->send_headers();

header("Content-type: application/json; charset=utf-8");
header("Cache-Control: no-cache, no-store, must-revalidate");

global $wpdb;
$table_name = $wpdb->prefix . 'bang_stats';

$click = @$_REQUEST["click"];
$referrer = @$_REQUEST["ref"];
$page = @$_REQUEST["page"];

$wpdb->insert(
  $table_name,
  array(
    'time' => current_time('mysql'),
    'click' => $click ?? "",
    'page' => $page ?? "",
    'referrer' => $referrer ?? "",
    'ip' => @$_SERVER['HTTP_X_FORWARDED_FOR'] ?? $_SERVER['REMOTE_ADDR'],
    'user_agent' => @$_SERVER['HTTP_USER_AGENT']
  )
);

$result = array();
$result["done"] = true;
echo json_encode($result);
