-- Table structure for table pages
CREATE TABLE IF NOT EXISTS `searches` (
  `id` bigint(11) unsigned NOT NULL AUTO_INCREMENT,
  `search_text` varchar(1024),
  `latitude` varchar(1024),
  `longitude` varchar(1024),
  `city` varchar(1024),
  `country` varchar(1024),
  `date_time` TIMESTAMP NOT NULL DEFAULT NOW(),
  PRIMARY KEY (`id`)
) ENGINE=InnoDB  DEFAULT CHARSET=utf8 AUTO_INCREMENT=3 ;
