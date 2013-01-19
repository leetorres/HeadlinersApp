-- Table structure for table pages
CREATE TABLE IF NOT EXISTS `twitter_users` (
  `id` bigint(11) unsigned NOT NULL AUTO_INCREMENT,
  `id_str` varchar(1024),
  `location` varchar(1024),
  `following` varchar(1024),
  `followers_count` varchar(1024),
  `listed_count` varchar(1024),
  `created_at` varchar(1024),
  `description` varchar(1024),
  `url` varchar(1024),
 
  PRIMARY KEY (`id`)
) ENGINE=InnoDB  DEFAULT CHARSET=utf8 AUTO_INCREMENT=3 ;
