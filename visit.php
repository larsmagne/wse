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
$table_name = $wpdb->prefix . 'wse_stats';

$click = @$_REQUEST["click"];
$referrer = @$_REQUEST["ref"];
$page = @$_REQUEST["page"];
$user_agent = @$_SERVER['HTTP_USER_AGENT'];
$title = @$_REQUEST["title"];

$result = array();
$result["done"] = true;

// Don't count logged-in users.
foreach ($_COOKIE as $key=>$val) {
  if (preg_match("/^wordpress_logged_in_/", $key) &&
      $val != "") {
    echo json_encode($result);
    exit;
  }
}

// Skip Googlebot, AHrefsbot, etc.
if (! preg_match("#bot/#i", $user_agent)) {
  $wpdb->insert(
    $table_name,
    array(
      'time' => current_time('mysql', true),
      'click' => $click ?? "",
      'page' => $page ?? "",
      'referrer' => $referrer ?? "",
      'ip' => @$_SERVER['HTTP_X_FORWARDED_FOR'] ?? $_SERVER['REMOTE_ADDR'],
      'user_agent' => $user_agent,
      'title' => $title
    )
  );
}

echo json_encode($result);
