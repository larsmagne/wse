<?php
require_once($_SERVER['DOCUMENT_ROOT'] . '/wp-config.php');
$wp->init();
$wp->parse_request();
$wp->query_posts();
$wp->register_globals();
$wp->send_headers();

header("Content-type: application/json; charset=utf-8");
header("Cache-Control: no-cache, no-store, must-revalidate");

// Check that the password is correct.
require_once(dirname(__FILE__) . '/password.php');
$password = @$_REQUEST["password"];
if ($password != wse_password())
  return;

global $wpdb;
$table_name = $wpdb->prefix . 'wse_stats';

$from_id = @$_REQUEST["from_id"];
if (! $from_id)
  $from_id = 0;
else
  $from_id = sprintf("%d", $from_id);

$from_comment_id = @$_REQUEST["from_comment_id"];
if (! $from_comment_id)
  $from_comment_id = 0;
else
  $from_comment_id = sprintf("%d", $from_comment_id);

// Select new stats data.
$sql = $wpdb->prepare("SELECT * FROM $table_name WHERE id > %d ORDER BY id",
                      $from_id);
$results = $wpdb->get_results($sql);

$data = array();
foreach ($results as $result) {
  $data[] = array($result->id, $result->time, $result->click,
                  $result->page, $result->referrer, $result->ip,
                  $result->user_agent, $result->title);
}

// Tidy up the database by deleting months-old data.
$old = date("Y-m-d H:i:s", time() - 31*24*60*60);
$wpdb->query("delete from $table_name where time < '$old'");

$output = array();
$output["data"] = $data;

// Also include comments in the data.
$comment_table = $wpdb->prefix . 'comments';
$cutoff = date("Y-m-d H:i:s", time() - 7*24*60*60);
$select = "select comment_id, comment_post_id, comment_author, comment_author_email, comment_author_url, comment_date_gmt, comment_content, comment_approved from $comment_table where ";

if ($from_comment_id) {
  $sql = $wpdb->prepare("$select comment_id > %s and comment_approved <> 'spam' order by comment_id",
                        $from_comment_id);
  $results = $wpdb->get_results($sql);
} else {
  $results = $wpdb->get_results("$select comment_date > '$cutoff' and comment_approved <> 'spam' order by comment_id");
}

$output["comments"] = $results;

echo json_encode($output);
