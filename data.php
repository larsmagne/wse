<?php
require_once($_SERVER['DOCUMENT_ROOT'] . '/wp-config.php');
$wp->init();
$wp->parse_request();
$wp->query_posts();
$wp->register_globals();
$wp->send_headers();

header("Content-type: application/json; charset=utf-8");
header("Cache-Control: no-cache, no-store, must-revalidate");

require_once(dirname(__FILE__) . '/password.php');
$password = @$_REQUEST["password"];
if ($password != bang_password())
  return;

global $wpdb;
$table_name = $wpdb->prefix . 'bang_stats';

$from_id = @$_REQUEST["from_id"];
if (! $from_id)
  $from_id = 0;
$from_id = sprintf("%d", $from_id);

$sql = $wpdb->prepare("SELECT * FROM $table_name WHERE id > %d ORDER BY id",
                      $from_id);
$results = $wpdb->get_results($sql);

$data = array();

foreach ($results as $result) {
  $data[] = array($result->id, $result->time, $result->click,
                  $result->page, $result->referrer, $result->ip,
                  $result->user_agent, $result->title);
}

$output = array();
$output["data"] = $data;
echo json_encode($output);
