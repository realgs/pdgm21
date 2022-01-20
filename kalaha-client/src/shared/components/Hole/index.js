import React from "react";
import cn from "classnames";

import styles from "./styles.module.scss";

export const Hole = ({ type, count, onClick, className }) => {
  return type === "normal" ? (
    <button className={cn(styles.hole, className)} onClick={onClick}>
      {count}
    </button>
  ) : (
    <div className={cn(styles.homeHole, className)}>{count}</div>
  );
};
