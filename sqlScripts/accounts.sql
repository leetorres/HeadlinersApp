-- Table structure for table pages
CREATE TABLE IF NOT EXISTS `accounts` (
  `id` bigint(11) unsigned NOT NULL AUTO_INCREMENT,
  `email` varchar(1024),
  `password` varchar(1024),
  `company` varchar(1024),
  PRIMARY KEY (`id`)
) ENGINE=InnoDB  DEFAULT CHARSET=utf8 AUTO_INCREMENT=3 ;
