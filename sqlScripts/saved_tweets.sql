-- Table structure for table pages
CREATE TABLE IF NOT EXISTS `saved_tweets` (
  `id` bigint(11) unsigned NOT NULL AUTO_INCREMENT,
  `id_str` varchar(1024),
  `fromuser_idstr` varchar(1024),
  `email` varchar(1024),
  PRIMARY KEY (`id`)
) ENGINE=InnoDB  DEFAULT CHARSET=utf8 AUTO_INCREMENT=3 ;
