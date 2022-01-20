import React from "react";

import styles from "./styles.module.scss";

export const MenuButton = ({ onClick, caption }) => (
  <button onClick={onClick} className={styles.button}>
    {caption}
  </button>
);
